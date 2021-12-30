use super::{
    hlir::Statement,
    expr::compile_expr,
    registers::{RegisterDescriptor, RegisterManager},
    stack::StackManager,
    AssemblyOutput, Memory,
};

pub fn compile_block<'source>(
    stack: &mut StackManager,
    registers: &mut RegisterManager,
    block: Vec<Statement>,
    bail_return_target: RegisterDescriptor,
    var_ctx: &[Memory],
) -> AssemblyOutput {
    let mut out = AssemblyOutput::new();
    for statement in block {
        out.extend(match statement {
            Statement::Return(expr) => {
                compile_expr(expr, bail_return_target, registers, stack, var_ctx, false)
            }
            Statement::Single(expr) => {
                compile_expr(expr, bail_return_target, registers, stack, var_ctx, true)
            }
        })
    }
    out
}
