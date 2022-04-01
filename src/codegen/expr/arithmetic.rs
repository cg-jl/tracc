use crate::{
    ast::ArithmeticOp,
    codegen::{
        assembly::{BitSize, Data, ImmutableRegister, Instruction},
        hlir::Expr,
        registers::UsageContext,
        AssemblyOutput, CompileWith, CompilerContext,
    },
};

use super::compile_expr_as_data;

pub fn compile_arithmetic_op(
    ctx: &mut CompilerContext,
    op: ArithmeticOp,
    lhs: Expr,
    rhs: Expr,
) -> AssemblyOutput {
    lhs.compile(ctx)
        .chain(ctx.checking_ignored(move |ctx, is_ignored| {
            if !is_ignored {
                let (compute_rhs, rhs_data) = compile_expr_as_data(ctx, rhs);

                match op {
                    ArithmeticOp::Add => compute_rhs.chain(ctx.using_register_mutably(
                        ctx.target,
                        BitSize::Bit32,
                        |_ctx, target| Instruction::Add {
                            target,
                            lhs: target.into(),
                            rhs: rhs_data,
                        },
                    )),
                    ArithmeticOp::Subtract => compute_rhs.chain(ctx.using_register_mutably(
                        ctx.target,
                        BitSize::Bit32,
                        |_ctx, target| Instruction::Sub {
                            target,
                            lhs: target.into(),
                            rhs: rhs_data,
                        },
                    )),
                    ArithmeticOp::Multiply => compute_rhs.chain(ctx.using_register_mutably(
                        ctx.target,
                        BitSize::Bit32,
                        |_ctx, target| Instruction::Sub {
                            target,
                            lhs: target.into(),
                            rhs: rhs_data,
                        },
                    )),
                    ArithmeticOp::Divide => compute_rhs.chain(ctx.using_register_mutably(
                        ctx.target,
                        BitSize::Bit32,
                        |_ctx, target| Instruction::Div {
                            target,
                            lhs: target.into(),
                            rhs: rhs_data,
                            signed: true,
                        },
                    )),
                    // NOTE: here I have a use for a `get_suitable_register` function that accepts a
                    // "locked" range (apart from the already existing).
                    ArithmeticOp::Modulo => {
                        compute_rhs.chain(ctx.locking_register(ctx.target, |ctx| {
                            let helper = if let Data::Register(ImmutableRegister(reg)) = rhs_data {
                                ctx.locking_register(reg.as_register_descriptor().unwrap(), |ctx| {
                                    // this will be the quotient register, that I'm going to throw
                                    ctx.registers.get_suitable_register(UsageContext::Normal)
                                })
                            } else {
                                ctx.registers.get_suitable_register(UsageContext::Normal)
                            };
                            // udiv helper, target (lhs), rhs_target
                            // msub target, helper, rhs_target, target (lhs)
                            ctx.using_register_mutably(helper, BitSize::Bit32, |ctx, helper| {
                                Instruction::Div {
                                    signed: false,
                                    target: helper,
                                    lhs: ctx.target.as_immutable(BitSize::Bit32),
                                    rhs: rhs_data,
                                }
                            })
                            .chain(ctx.using_register_mutably(
                                ctx.target,
                                BitSize::Bit32,
                                |ctx, target| {
                                    let (rhs_reg, put_rhs_in_reg) =
                                        if let Data::Register(ImmutableRegister(reg)) = rhs_data {
                                            (
                                                reg.as_register_descriptor().unwrap(),
                                                AssemblyOutput::new(),
                                            )
                                        } else {
                                            let reg = ctx
                                                .registers()
                                                .get_suitable_register(UsageContext::Normal);
                                            (
                                                reg,
                                                ctx.using_register_mutably(
                                                    reg,
                                                    BitSize::Bit32,
                                                    |_ctx, reg| Instruction::Mov {
                                                        target: reg,
                                                        source: rhs_data,
                                                    },
                                                ),
                                            )
                                        };
                                    put_rhs_in_reg.chain_single(Instruction::MSub {
                                        target,
                                        multiplicand: helper.as_immutable(BitSize::Bit32),
                                        multiplier: rhs_reg.as_immutable(BitSize::Bit32),
                                        minuend: target.into(),
                                    })
                                },
                            ))
                        }))
                    }
                }
            } else {
                ctx.with_ignoring(true, |ctx| rhs.compile(ctx))
            }
        }))
}
