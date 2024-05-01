use std::collections::HashSet;

use crate::ir::analysis;

use super::{cleanup, BasicBlock, Binding, BlockBinding, Statement, Value, IR};

pub mod redefine;

pub fn values_mut<'b>(block: &'b mut BasicBlock) -> impl Iterator<Item = &'b mut Value> + 'b {
    block.statements.iter_mut().filter_map(|stmt| {
        if let Statement::Assign { value, .. } = stmt {
            Some(value)
        } else {
            None
        }
    })
}

/// Replaces phi arguments for `prev` with `new`, making `new`'s value prevalent if it is already there:
///
///
/// # Safety
/// `prev` and `new` must satisfy:
/// ϕ(`prev`, `new`) = ϕ(`new`)
/// i.e. The most up-to-date value between `prev` and `new` is the one from `new`, regardless of
/// branching. As an example, this can happen when inlining blocks.
///
/// # Example:
///
/// Say the IR is something like this:
/// ```txt
/// BB0:
///     %1 = 0
///     br BB1
/// BB1:
///     %2 = 0
///     br BB2
/// BB2:
///     %3 = ϕ(BB1 -> %1, BB2 -> %2, BBN -> %N)
///     ret %3
/// ...
/// BBN:
///     ...
///     br-cond BBM, BB2
/// ```
///
/// We know that ϕ(BB1, BB2) = ϕ(BB2) since to come from BB0 we have to pass through BB2, so the
/// most updated value is the one from BB2.
/// ```no_run
/// unsafe { replace_phi_source(ir, BlockBinding(0), BlockBinding(2)) };
/// ```
/// Will transform the IR to look like this:
/// ```txt
/// ...
/// BB2:
///     %3 = ϕ(BB2 -> %2, BBN -> %N)
///     ret %3
/// ...
/// ```
pub unsafe fn replace_phi_source(ir: &mut IR, prev: BlockBinding, new: BlockBinding) {
    tracing::trace!(target: "refactor::replace_phi", "Assuming ϕ({prev}, {new}) = ϕ({new})");
    for value in ir.code.iter_mut().flat_map(values_mut) {
        if let Value::Phi { nodes } = value {
            if nodes.iter().find(|phi| phi.block_from == new).is_some() {
                nodes.retain(|phi| phi.block_from != prev);
                let nodes = std::mem::take(nodes);
                std::mem::replace(value, cleanup::recount_phi(nodes));
            } else if let Some(found) = nodes.iter_mut().find(|phi| phi.block_from == prev) {
                found.block_from = new;
            }
        }
    }
    tracing::trace!(target: "irshow::replace_phi", "ir: {ir:?}");
}

/// Remove a binding from the IR
///
/// # Safety
/// The binding must either be already allocated to a read-only register or not used elsewhere
pub unsafe fn remove_binding(ir: &mut IR, target: Binding) {
    for block in &mut ir.code {
        if let Some(index) = block
            .statements
            .iter()
            .enumerate()
            .find_map(|(index, stmt)| {
                if let Statement::Assign {
                    index: assign_target,
                    ..
                } = stmt
                {
                    if assign_target == &target {
                        return Some(index);
                    }
                }
                None
            })
        {
            block.statements.remove(index);
        }
    }
}

/// Remove a block from phi nodes, since it can't be reached
/// # Safety
/// The block must not be reached in any way.
pub unsafe fn remove_block_from_phis(ir: &mut IR, target: BlockBinding) {
    for stmt in ir.code.iter_mut().flat_map(|b| &mut b.statements) {
        if let Statement::Assign { index, value } = stmt {
            let new_value = match std::mem::take(value) {
                Value::Phi { mut nodes } => {
                    nodes.retain(|phi| phi.block_from != target);
                    match nodes.len() {
                        0 => Value::Uninit,
                        1 => Value::Binding(nodes[0].value),
                        _ => Value::Phi { nodes },
                    }
                }
                other => other,
            };
            std::mem::replace(value, new_value);
        }
    }
}

/// Remove a block from phi nodes, since it can't be reached
/// # Safety
/// The block must not be a direct ancestor from `in`.
pub unsafe fn remove_block_from_phis_in(ir: &mut IR, target: BlockBinding, r#in: BlockBinding) {
    for stmt in &mut ir[r#in].statements {
        if let Statement::Assign { index, value } = stmt {
            let new_value = match std::mem::take(value) {
                Value::Phi { mut nodes } => {
                    nodes.retain(|phi| phi.block_from != target);
                    match nodes.len() {
                        0 => Value::Uninit,
                        1 => Value::Binding(nodes[0].value),
                        _ => Value::Phi { nodes },
                    }
                }
                other => other,
            };
            std::mem::replace(value, new_value);
        }
    }
}

/// Remove a block from the IR
///
/// # Safety
/// The block must not be referred by any of the blocks that come after its index
/// in the IR's vector.
pub unsafe fn remove_block(ir: &mut IR, target: BlockBinding) -> BasicBlock {
    tracing::trace!(target: "irshow::refactor", "IR before removal of {target}: {ir:?}");
    let BlockBinding(index) = target;
    unsafe { remove_block_from_phis(ir, target) };

    // shift the names of the blocks to the right to be 1 less.
    for i in index..ir.code.len() {
        rename_block(ir, BlockBinding(i + 1), BlockBinding(i));
    }

    // now we can remove that basic block
    ir.code.remove(index)
}

/// Rename a block inside a block end.
///
/// # Safety
/// The new block name must not collide with other block names
pub unsafe fn end_rename_block(
    end: &mut super::BlockEnd,
    target: BlockBinding,
    replace_with: BlockBinding,
) {
    match end {
        super::BlockEnd::Branch(ref mut branch) => match branch {
            super::Branch::Unconditional {
                target: branch_target,
            } => {
                if target == *branch_target {
                    *branch_target = replace_with;
                }
            }
            super::Branch::Conditional {
                flag: _,
                target_true,
                target_false,
            } => {
                if *target_true == target {
                    *target_true = replace_with;
                }

                if *target_false == target {
                    *target_false = replace_with;
                }
            }
        },
        // nothing to do here
        super::BlockEnd::Return(_) => (),
    }
}

/// Rename a block
///
/// # Safety
/// The new block name must not collide with other blocks.
pub unsafe fn rename_block(ir: &mut IR, target: BlockBinding, replace_with: BlockBinding) {
    tracing::trace!(target: "refactor::rename", "Renaming {target} -> {replace_with}");

    for value in ir.code.iter_mut().flat_map(values_mut) {
        if let Value::Phi { nodes } = value {
            if let Some(phi) = nodes.iter_mut().find(|phi| phi.block_from == target) {
                phi.block_from = replace_with;
                #[cfg(debug_assertions)]
                {
                    let old_len = nodes.len();
                    nodes.dedup_by_key(|phi| phi.block_from);
                    assert!(
                        nodes.len() == old_len,
                        "Block from should have been previously updated,
otherwise we end with an invalid phi node with two possible values for a single precedent block"
                    );
                }
            }
        }
    }

    for block in &mut ir.code {
        end_rename_block(&mut block.end, target, replace_with);
    }

    for x in ir
        .forward_map
        .values_mut()
        .flatten()
        .chain(ir.backwards_map.values_mut().flatten())
        .chain(ir.function_entrypoints.iter_mut())
    {
        if x == &target {
            *x = replace_with;
        }
    }

    if let Some(values) = ir.forward_map.remove(&target) {
        ir.forward_map.insert(replace_with, values);
    }

    if let Some(values) = ir.backwards_map.remove(&target) {
        ir.backwards_map.insert(replace_with, values);
    }
}
