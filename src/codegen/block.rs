use super::{
    assembly::{BitSize, Branch, Condition, Data, Instruction},
    hlir::{Expr, Statement},
    labels::LabelGenerator,
    AssemblyOutput, CompileWith, CompilerContext, LoopEnv,
};

impl CompileWith<CompilerContext> for Vec<Statement> {
    fn compile(self, ctx: &mut CompilerContext) -> super::AssemblyOutput {
        let mut out = AssemblyOutput::new();
        // I'll stick this here for the moment
        for statement in super::constant_folding::reorder_and_fold_block(self) {
            out.extend(match statement {
                Statement::Return(expr) => expr.compile(ctx).chain_single(Instruction::Ret),
                Statement::Single(expr) => ctx.with_ignoring(true, |ctx| expr.compile(ctx)),
                Statement::IfStatement {
                    condition,
                    true_branch,
                    false_branch,
                } => {
                    match condition {
                        // I can predict the branch and say that it's only going to execute the false
                        // branch
                        Expr::Constant(0) => false_branch
                            .map(|false_branch| false_branch.compile(ctx))
                            .unwrap_or_else(AssemblyOutput::new),
                        // I can predict the branch and say that it's only going to hit the true branch
                        Expr::Constant(_) => true_branch.compile(ctx),
                        mut condition => {
                            let else_label = LabelGenerator::global().new_label();
                            condition.set_branch_depends_on_result();
                            let opt_relational_op = condition.relational_op();
                            // compute the expression into the register
                            let unchanged_part = ctx
                                .with_ignoring(false, |ctx| condition.compile(ctx))
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
                                        register: ctx.target.as_immutable(BitSize::Bit32),
                                        data: Data::immediate(0, BitSize::Bit32),
                                    })
                                    .chain_single(
                                        Branch::Conditional {
                                            condition: Condition::Equals,
                                            label: else_label,
                                        },
                                    )
                                })
                                .chain(true_branch.compile(ctx));
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
                                    .chain(false_branch.compile(ctx))
                                    .chain_single(skip_false_branch)
                            } else {
                                // otherwise we just append the label after the true branch
                                unchanged_part.chain_single(else_label)
                            }
                        }
                    }
                }
                Statement::Loop {
                    condition,
                    block,
                    condition_at_end,
                } => {
                    let env = LoopEnv {
                        start: LabelGenerator::global().new_label(),
                        end: LabelGenerator::global().new_label(),
                    };
                    let start: AssemblyOutput = env.start.into();
                    let condition = ctx.with_ignoring(false, |ctx| condition.compile(ctx));
                    let body = ctx.with_loop(env, |ctx| block.compile(ctx));
                    let compare = Instruction::Cmp {
                        register: ctx.target.as_immutable(BitSize::Bit32),
                        data: Data::immediate(0, BitSize::Bit32),
                    };
                    start
                        .chain(if condition_at_end {
                            body.chain(condition).chain_single(compare).chain_single(
                                // conditionally go back to the start (if condition, jump back to
                                // start)
                                Branch::Conditional {
                                    condition: Condition::NotEquals,
                                    label: env.start,
                                },
                            )
                        } else {
                            condition
                                .chain_single(compare)
                                // conditionally execute the body (if not condition, jump to end)
                                .chain_single(Branch::Conditional {
                                    condition: Condition::Equals,
                                    label: env.end,
                                })
                                .chain(body)
                        })
                        .chain_single(env.end)
                }
            })
        }
        out
    }
}

// pub fn compile_block(
//     stack: &mut StackManager,
//     registers: &mut RegisterManager,
//     block: Vec<Statement>,
//     bail_return_target: RegisterDescriptor,
//     var_ctx: &[Memory],
// ) -> AssemblyOutput {
// }
