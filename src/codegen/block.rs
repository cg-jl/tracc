use super::{
    assembly::{BitSize, Branch, Condition, Data, Instruction, Label},
    expr::compile_expr,
    hlir::{Expr, LoopAllowedStatement, Statement},
    labels::LabelGenerator,
    registers::{RegisterDescriptor, RegisterManager},
    stack::StackManager,
    AssemblyOutput, Memory,
};

pub fn compile_statement(
    stack: &mut StackManager,
    registers: &mut RegisterManager,
    statement: Statement,
    bail_return_target: RegisterDescriptor,
    var_ctx: &[Memory],
) -> AssemblyOutput {
    match statement {
        Statement::Return(expr) => {
            compile_expr(expr, bail_return_target, registers, stack, var_ctx, false)
                .chain_single(Instruction::Ret)
        }
        Statement::Single(expr) => {
            compile_expr(expr, bail_return_target, registers, stack, var_ctx, true)
        }
        Statement::Loop { condition, body } => match condition {
            // ignore the whole loop, because it never runs.
            Expr::Constant(0) => AssemblyOutput::new(),
            Expr::Constant(_) => {
                // it's a while(true) loop, so just compile the block and jump back again
                // TODO: analyze whether the block has a sure return (to remove the jumps)
                let start = LabelGenerator::global().new_label();
                let end = LabelGenerator::global().new_label();
                AssemblyOutput::singleton(start)
                    .chain(compile_loop_block(
                        stack,
                        registers,
                        body,
                        bail_return_target,
                        var_ctx,
                        start,
                        end,
                    ))
                    .chain_single(Branch::Unconditional {
                        register: None,
                        label: start,
                    })
            }
            cond => {
                let start = LabelGenerator::global().new_label();
                let end = LabelGenerator::global().new_label();
                // we'll reuse the target register for both the condition and the block.
                // # start the loop
                AssemblyOutput::singleton(start)
                    // # compute the condition expression
                    .chain(compile_expr(
                        cond,
                        bail_return_target,
                        registers,
                        stack,
                        var_ctx,
                        false,
                    ))
                    // # compare and conditionally bail out
                    .chain_single(Instruction::Cmp {
                        register: bail_return_target.as_immutable(BitSize::Bit32),
                        data: Data::immediate(0, BitSize::Bit32),
                    })
                    .chain_single(Branch::Conditional {
                        condition: Condition::Equals,
                        label: end,
                    })
                    // # otherwise follow through the block and jump back
                    .chain(compile_loop_block(
                        stack,
                        registers,
                        body,
                        bail_return_target,
                        var_ctx,
                        start,
                        end,
                    ))
                    // # end the loop
                    .chain_single(end)
            }
        },
        Statement::IfStatement {
            condition,
            true_branch,
            false_branch,
        } => {
            match condition {
                // I can predict the branch and say that it's only going to execute the false
                // branch
                Expr::Constant(0) => false_branch
                    .map(|false_branch| {
                        compile_block(stack, registers, false_branch, bail_return_target, var_ctx)
                    })
                    .unwrap_or_else(AssemblyOutput::new),
                // I can predict the branch and say that it's only going to hit the true branch
                Expr::Constant(_) => {
                    compile_block(stack, registers, true_branch, bail_return_target, var_ctx)
                }
                mut condition => {
                    let else_label = LabelGenerator::global().new_label();
                    condition.set_branch_depends_on_result();
                    let opt_relational_op = condition.relational_op();
                    // compute the expression into the register
                    let unchanged_part = compile_expr(
                        condition,
                        bail_return_target,
                        registers,
                        stack,
                        var_ctx,
                        false,
                    )
                    // if the comparison fails, jump to the else label
                    .chain(if let Some(constraint) = opt_relational_op {
                        // if the condition we just computed is a relational operation, then we
                        // can do the conditional jump directly
                        AssemblyOutput::singleton(Branch::Conditional {
                            condition: constraint.antidote().to_condition(),
                            label: else_label,
                        })
                    } else {
                        // otherwise we want to emit a compare instruction first
                        AssemblyOutput::singleton(Instruction::Cmp {
                            register: bail_return_target.as_immutable(BitSize::Bit32),
                            data: Data::Immediate(0),
                        })
                        .chain_single(Branch::Conditional {
                            condition: Condition::Equals,
                            label: else_label,
                        })
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
            }
        }
    }
}

pub fn compile_loop_allowed_statement(
    stack: &mut StackManager,
    registers: &mut RegisterManager,
    statement: LoopAllowedStatement,
    bail_return_target: RegisterDescriptor,
    var_ctx: &[Memory],
    loop_start: Label,
    loop_end: Label,
) -> AssemblyOutput {
    match statement {
        LoopAllowedStatement::RegularStatement(statement) => compile_statement(
            stack,
            registers,
            super::constant_folding::reorder_and_fold_statement(statement),
            bail_return_target,
            var_ctx,
        ),
        // jump to the beginning of the loop
        LoopAllowedStatement::Continue => Branch::Unconditional {
            register: None,
            label: loop_start,
        }
        .into(),
        // bail out of the loop.
        LoopAllowedStatement::Break => Branch::Unconditional {
            register: None,
            label: loop_end,
        }
        .into(),
    }
}

pub fn compile_loop_block(
    stack: &mut StackManager,
    registers: &mut RegisterManager,
    block: Vec<LoopAllowedStatement>,
    bail_return_target: RegisterDescriptor,
    var_ctx: &[Memory],
    loop_start: Label,
    loop_end: Label,
) -> AssemblyOutput {
    let mut out = AssemblyOutput::new();
    for statement in block {
        out.extend(compile_loop_allowed_statement(
            stack,
            registers,
            statement,
            bail_return_target,
            var_ctx,
            loop_start,
            loop_end,
        ));
    }
    out
}

pub fn compile_block(
    stack: &mut StackManager,
    registers: &mut RegisterManager,
    block: Vec<Statement>,
    bail_return_target: RegisterDescriptor,
    var_ctx: &[Memory],
) -> AssemblyOutput {
    let mut out = AssemblyOutput::new();
    // I'll stick this here for the moment
    for statement in super::constant_folding::reorder_and_fold_block(block) {
        out.extend(compile_statement(
            stack,
            registers,
            statement,
            bail_return_target,
            var_ctx,
        ));
    }
    out
}
