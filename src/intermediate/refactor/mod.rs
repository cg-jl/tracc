use super::{IR, BlockBinding};

pub mod redefine;

pub fn rename_block(ir: &mut IR, target: BlockBinding, replace_with: BlockBinding) {
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
