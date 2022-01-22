use crate::intermediate::{BasicBlock, Binding, IRCode, Statement, Value};

pub mod assembly;
mod memory;

/// find the definition for a particular binding in a block
fn find_definition_in_block(block: &BasicBlock, binding: Binding) -> Option<&Value> {
    for stmt in &block.statements {
        if let Statement::Assign { index, value } = stmt {
            if index == &binding {
                return Some(value);
            }
        }
    }
    None
}

/// find the definition for a particular binding in the code
fn find_definition_in_code(code: &IRCode, binding: Binding) -> Option<&Value> {
    code.iter()
        .map(|block| find_definition_in_block(block, binding))
        .fold(None, Option::or)
}
