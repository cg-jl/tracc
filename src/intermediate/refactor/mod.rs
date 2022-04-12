use super::{BasicBlock, BlockBinding, Statement, Value, IR};

pub mod redefine;

/// Remove a block from the IR
///
/// # Safety
/// The block must not be referred by any of the blocks that come after its index
/// in the IR's vector.
pub unsafe fn remove_block(ir: &mut IR, target: BlockBinding) -> BasicBlock {
    let BlockBinding(index) = target;

    // shift the names of the blocks to the right to be 1 less.
    for i in index + 1..ir.code.len() {
        rename_block(ir, BlockBinding(i), BlockBinding(i - 1));
    }

    // now we can remove that basic block
    ir.code.remove(index)
}

/// Rename a block
///
/// # Safety
/// The new block name must not collide with other blocks.
pub unsafe fn rename_block(ir: &mut IR, target: BlockBinding, replace_with: BlockBinding) {
    // rename refs in the code
    for block in ir.code.iter_mut() {
        match block.end {
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
    // rename refs in the branching maps
    for list in ir
        .forward_map
        .values_mut()
        .chain(ir.backwards_map.values_mut())
    {
        for x in list.iter_mut() {
            if x == &target {
                *x = replace_with;
            }
        }
    }

    if let Some(values) = ir.forward_map.remove(&target) {
        ir.forward_map.insert(replace_with, values);
    }

    if let Some(values) = ir.backwards_map.remove(&target) {
        ir.backwards_map.insert(replace_with, values);
    }
}
