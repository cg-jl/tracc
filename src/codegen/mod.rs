use std::collections::HashSet;

use crate::intermediate::{BasicBlock, BlockBinding, BlockEnd, PhiDescriptor, IR};
use crate::intermediate::{Binding, IRCode, Statement};

pub mod assembly;
pub mod has_binding;
pub mod memory;

/// find the definition for a particular binding in a block
fn find_definition_in_block(block: &BasicBlock, binding: Binding) -> Option<usize> {
    block
        .statements
        .iter()
        .enumerate()
        .find_map(|(statement_index, statement)| {
            if let Statement::Assign { index, value } = statement {
                if index == &binding {
                    return Some(statement_index);
                }
            }
            None
        })
}

/// find the definition for a particular binding in the code
fn find_definition_in_code(code: &IRCode, binding: Binding) -> Option<(usize, usize)> {
    code.iter().enumerate().find_map(|(block_index, block)| {
        find_definition_in_block(block, binding)
            .map(|statement_index| (block_index, statement_index))
    })
}

/// Find the leafs where a binding is last used
fn find_binding_last_used(
    code: &[BasicBlock],
    binding: Binding,
    definition_block: BlockBinding,
) -> HashSet<BlockBinding> {
    let mut result = HashSet::new();

    let mut queue = vec![definition_block];
    let mut visited = HashSet::new();

    while !queue.is_empty() {
        let come_from = queue.pop().unwrap();
        if !visited.contains(&come_from) {
            visited.insert(come_from);
            if let BlockEnd::Branch(branch) = code[come_from.0].end {
                for target in branch.branch_list() {
                    use has_binding::BindingUsage;
                    if !code[target.0].uses_binding(binding) {
                        result.insert(come_from);
                    } else {
                        queue.push(target);
                    }
                }
            }
        }
    }

    result
}

// leaf blocks are blocks that end in a `ret`.
fn find_leaf_blocks(code: &[BasicBlock]) -> impl Iterator<Item = BlockBinding> + '_ {
    code.iter().enumerate().filter_map(|(block_index, block)| {
        if let BlockEnd::Return(_) = block.end {
            Some(BlockBinding(block_index))
        } else {
            None
        }
    })
}
