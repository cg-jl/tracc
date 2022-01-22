use crate::intermediate::BasicBlock;
use crate::intermediate::{Binding, IRCode, Statement};

pub mod assembly;
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
