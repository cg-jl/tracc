use std::collections::HashMap;

use super::{
    expr::compile_expr,
    registers::{RegisterDescriptor, RegisterManager},
    stack::StackManager,
    AssemblyOutput, Memory,
};

use crate::{
    ast::{BinaryOp, Expr, Statement},
    error::Span,
};

pub fn compile_block<'source>(
    stack: &mut StackManager,
    registers: &mut RegisterManager,
    block: Vec<(Statement<'source>, Span)>,
    bail_return_target: RegisterDescriptor,
    var_idxs: &HashMap<&'source str, usize>,
    var_ctx: &[Memory],
) -> AssemblyOutput {
    let mut out = AssemblyOutput::new();
    for (statement, _) in block {
        out.extend(match statement {
            Statement::Return((expr, _)) => compile_expr(
                expr,
                bail_return_target,
                registers,
                stack,
                var_idxs,
                var_ctx,
                false,
            ),
            Statement::DeclareVar { init, name } => {
                // ignore it since it has already served its purpose.
                if let Some(init) = init {
                    compile_expr(
                        Expr::Binary {
                            operator: BinaryOp::Assignment { op: None },
                            lhs: (Box::new(Expr::Variable { name }), Span::new(0)),
                            rhs: (Box::new(init.0), init.1),
                        },
                        bail_return_target,
                        registers,
                        stack,
                        var_idxs,
                        var_ctx,
                        false,
                    )
                } else {
                    AssemblyOutput::new()
                }
            }
            Statement::SingleExpr((expr, _)) => compile_expr(
                expr,
                bail_return_target,
                registers,
                stack,
                var_idxs,
                var_ctx,
                true,
            ),
        })
    }
    out
}
