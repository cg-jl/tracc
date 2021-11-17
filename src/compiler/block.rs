use super::expr::compile_expr;
use super::registers::{RegisterDescriptor, RegisterManager};
use super::stack::StackManager;
use super::AssemblyOutput;
use crate::ast::Block;
use crate::ast::Statement;

// NOTE: when I specify mutable and immutable registers; this one has to be mutable
pub fn compile_block(
    stack: &mut StackManager,
    registers: &mut RegisterManager,
    block: &Block,
    bail_return_register: RegisterDescriptor,
) -> AssemblyOutput {
    let mut out = AssemblyOutput::new();
    for statement in &block.0 {
        out.extend(match statement {
            Statement::Return(expr) => compile_expr(expr, bail_return_register, registers, stack),
        })
    }
    out
}
