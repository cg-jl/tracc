use super::{
    expr::{compile_expr, CompileExprError},
    registers::{RegisterDescriptor, RegisterManager},
    stack::StackManager,
    AssemblyOutput, Memory,
};
use crate::ast::{Block, Statement};

// NOTE: when I specify mutable and immutable registers; this one has to be mutable
pub fn compile_block(
    stack: &mut StackManager,
    registers: &mut RegisterManager,
    block: Block,
    bail_return_target: RegisterDescriptor,
    var_ctx: &[Memory],
) -> Result<AssemblyOutput, CompileExprError> {
    let mut out = AssemblyOutput::new();
    for statement in block.0 {
        out.extend(match statement {
            Statement::Return(expr) => {
                compile_expr(expr, bail_return_target, registers, stack, var_ctx, false)?
            }
            Statement::DeclareVar { .. } => {
                unreachable!("declaring variables should not get to assembly phase")
            }
            Statement::SingleExpr(expr) => {
                compile_expr(expr, bail_return_target, registers, stack, var_ctx, true)?
            }
        })
    }
    Ok(out)
}
