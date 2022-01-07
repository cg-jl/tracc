use super::{
    assembly::{BitSize, Branch, Condition, Data, Instruction},
    expr::compile_expr,
    hlir::Statement,
    labels::LabelGenerator,
    registers::{RegisterDescriptor, RegisterManager},
    stack::StackManager,
    AssemblyOutput, Memory,
};

pub fn compile_block(
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
            Statement::IfStatement {
                condition,
                true_branch,
                false_branch,
            } => {
                let else_label = LabelGenerator::global().new_label();
                // compute the expression into the register
                let unchanged_part = compile_expr(
                    condition,
                    bail_return_target,
                    registers,
                    stack,
                    var_ctx,
                    false,
                )
                // if it results in a zero, jump to the else label
                .chain_single(Instruction::Cmp {
                    register: bail_return_target.as_immutable(BitSize::Bit32),
                    data: Data::immediate(0, BitSize::Bit32),
                })
                .chain_single(Branch::Conditional {
                    condition: Condition::Equals,
                    label: else_label,
                })
                .chain(compile_block(
                    stack,
                    registers,
                    true_branch,
                    bail_return_target,
                    var_ctx,
                ));
                if let Some(false_branch) = false_branch {
                    // if there is another part to execute, we have to generate another jump so we
                    // can skip the false branch when we went through the true branch
                    let skip_false_branch = LabelGenerator::global().new_label();
                    unchanged_part
                        .chain_single(Branch::Unconditional {
                            register: None,
                            label: skip_false_branch,
                        })
                        .chain_single(else_label)
                        .chain(compile_block(
                            stack,
                            registers,
                            false_branch,
                            bail_return_target,
                            var_ctx,
                        ))
                        .chain_single(skip_false_branch)
                } else {
                    // otherwise we just append the label after the true branch
                    unchanged_part.chain_single(else_label)
                }
            }
        })
    }
    out
}
