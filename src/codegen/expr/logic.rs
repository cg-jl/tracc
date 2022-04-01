use crate::{
    ast::LogicOp,
    codegen::{
        assembly::{BitSize, Branch, Condition, Data, Instruction},
        hlir::Expr,
        labels::LabelGenerator,
        AssemblyOutput, CompileWith, CompilerContext,
    },
};

pub fn compile_logic_op(
    ctx: &mut CompilerContext,
    logicop: LogicOp,
    lhs: Expr,
    rhs: Expr,
) -> AssemblyOutput {
    // NOTE: revise this when supporting floating point as those might use
    // different registers and we might not be able to just use the target
    // register as the comparing one.
    // NOTE: there is a case in ignoring expressions in which if the whole
    // thing is ignored and the right side is ignorable then we can forget
    // about the right side. Otherwise, we want to check if the left side is
    // ignorable and if it *is*, we will compile just the right side, if none
    // of them are ignorable then we have to output the branch. Here, for
    // simplicity, I've omitted this optimization and I'm ignoring the
    // `is_ignored` flag and compiling as if that flag was `false`.
    match logicop {
        LogicOp::And => match lhs {
            // if there is a zero on the left side, we won't even bother checking the right
            // side, we know it's never going to happen.
            Expr::Constant(0) => ctx.empty_on_ignore(|ctx| {
                ctx.using_register_mutably(ctx.target, BitSize::Bit32, |_ctx, target| {
                    Instruction::Mov {
                        target,
                        source: Data::Immediate(0),
                    }
                })
            }),
            // if there is a non-zero constant on the left side, we use 1 && A = A and just
            // compile the right side.
            Expr::Constant(_) => rhs.compile(ctx),
            // otherwise, let's take a look at the right side
            lhs => match rhs {
                // if there's a zero on the RIGHT side, we want to compile the left side
                // knowing that the result of it will be ignored, since we will put a zero on
                // the result no matter what the lhs results in.
                Expr::Constant(0) => {
                    ctx.with_ignoring(true, |ctx| rhs.compile(ctx))
                        .chain(ctx.empty_on_ignore(|ctx| {
                            ctx.using_register_mutably(
                                ctx.target,
                                BitSize::Bit32,
                                |_ctx, target| Instruction::Mov {
                                    target,
                                    source: Data::Immediate(0),
                                },
                            )
                        }))
                }
                // if there is a non-zero constant on the right side, we apply again A && 1 = A
                // and just compile the left side.
                Expr::Constant(_) => lhs.compile(ctx),
                // otherwise, we'll let the processor decide at runtime.
                rhs => {
                    // binaryop_logicand rx:
                    //  <rx <- lhs>
                    //  cmp rx, #0
                    //  beq end
                    //  <rx <- rhs>
                    // end:
                    let end = LabelGenerator::global().new_label();
                    ctx.with_ignoring(false, |ctx| lhs.compile(ctx)) // do not ignore so that I can compare it.
                        .chain_single(Instruction::Cmp {
                            register: ctx.target.as_immutable(BitSize::Bit32),
                            data: Data::Immediate(0),
                        })
                        .chain_single(Instruction::Branch(Branch::Conditional {
                            condition: Condition::Equals,
                            label: end,
                        }))
                        .chain(rhs.compile(ctx)) // here I don't care if this whole expression was set to be ignored.
                        .chain_single(end)
                }
            },
        },

        LogicOp::Or => match lhs {
            // if lhs is a zero: 0 || B = B, then just compile rhs
            Expr::Constant(0) => rhs.compile(ctx),
            // if lhs is non-zero, then we can forget about B as it is not going to be evaluated,
            // and just load the constant
            Expr::Constant(x) => ctx.empty_on_ignore(|ctx| {
                ctx.using_register_mutably(ctx.target, BitSize::Bit32, |_ctx, target| {
                    Instruction::Mov {
                        target,
                        source: Data::Immediate(x as u64),
                    }
                })
            }),
            // otherwise, take a look at the right hand side
            lhs => match rhs {
                // if rhs is zero, that means L || 0 == L and we just need the value of L
                Expr::Constant(0) => lhs.compile(ctx),
                // if rhs is non-zero, that means L || X == X, (X != 0) but we want to compile L if it's
                // needed so we just add the ignored flag to it
                Expr::Constant(x) => {
                    ctx.with_ignoring(true, |ctx| lhs.compile(ctx))
                        .chain(ctx.empty_on_ignore(|ctx|
                            // NOTE: this bit size might be changed to the target's bit size
                            ctx.using_register_mutably(
                                ctx.target,
                                BitSize::Bit32,
                                |_ctx, target| Instruction::Mov {
                                    target,
                                    source: Data::Immediate(x as u64),
                                },
                            )))
                }
                // otherwise, we will compile the branch
                rhs => {
                    // binary_logicor rx:
                    //  <rx <- lhs>
                    //  cmp rx, #0
                    //  bne end
                    //  <rx <- rhs>
                    //end:
                    let end = LabelGenerator::global().new_label();
                    ctx.with_ignoring(false, |ctx| lhs.compile(ctx))
                        .chain_single(Instruction::Cmp {
                            register: ctx.target.as_immutable(BitSize::Bit32),
                            data: Data::Immediate(0),
                        })
                        .chain_single(Instruction::Branch(Branch::Conditional {
                            condition: Condition::NotEquals,
                            label: end,
                        }))
                        .chain(rhs.compile(ctx))
                        .chain_single(end)
                }
            },
        },
    }
}
