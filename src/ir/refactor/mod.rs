use super::{BasicBlock, Binding, BlockBinding, Statement, Value, IR};

pub mod redefine;

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
pub unsafe fn remove_unreached_block_from_phi_statements(ir: &mut IR, target: BlockBinding) {
    for block in &mut ir.code {
        for statement in &mut block.statements {
            if let Statement::Assign {
                index: _,
                value: Value::Phi { nodes },
            } = statement
            {
                for i in (0..nodes.len()).rev() {
                    if nodes[i].block_from == target {
                        nodes.remove(i);
                    }
                }
            }
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

    // shift the names of the blocks to the right to be 1 less.
    for i in index..ir.code.len() {
        tracing::trace!(target: "fold::remove", "renaming {} -> {}", BlockBinding(i + 1), BlockBinding(i));
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
    // rename refs in the code
    for block in ir.code.iter_mut() {
        for statement in &mut block.statements {
            // rename phi nodes
            if let Statement::Assign {
                value: Value::Phi { nodes },
                ..
            } = statement
            {
                for node in nodes {
                    if node.block_from == target {
                        node.block_from = replace_with;
                    }
                }
            }
        }
        end_rename_block(&mut block.end, target, replace_with);
    }

    // rename refs in the branching maps & function entrypoints
    for x in ir
        .forward_map
        .values_mut()
        .flat_map(|x| x.iter_mut())
        .chain(ir.backwards_map.values_mut().flat_map(|x| x.iter_mut()))
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
