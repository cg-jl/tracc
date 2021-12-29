use std::collections::HashMap;

use crate::{
    ast::{ArithmeticOp, Expr},
    codegen::{
        assembly::{BitSize, Data, ImmutableRegister, Instruction, Memory},
        registers::{RegisterDescriptor, RegisterManager, UsageContext},
        stack::StackManager,
        AssemblyOutput,
    },
};

use super::compile_expr;

pub fn compile_arithmetic_op<'source>(
    arithmop: ArithmeticOp,
    target: RegisterDescriptor,
    lhs: Expr<'source>,
    rhs: Expr<'source>,
    registers: &mut RegisterManager,
    stack: &mut StackManager,
    var_idxs: &HashMap<&'source str, usize>,
    var_ctx: &[Memory],
    is_ignored: bool,
) -> AssemblyOutput {
    // no weird logic optimizations here, just do the computing on both sides and then join them
    // together
    // NOTE: when rhs is (or contains) a function call, lhs can't use target and must use a call-safe register
    // instead. might need to rewrite this in a fashion that allows the compiler to decide what to
    // compute first in order to minimize the register usage.
    // also, take into account that if the expression is ignored, then there's no need to save any
    // of the results.
    // as currently we don't support calling functions, we are safe using `target`.

    let (lhs, rhs) = super::reorder_binary_expr(arithmop, lhs, rhs);
    compile_expr(lhs, target, registers, stack, var_idxs, var_ctx, is_ignored).chain(
        if !is_ignored {
            let (compute_rhs, rhs_data) = if let Expr::Constant(b) = rhs {
                (AssemblyOutput::new(), Data::Immediate(b as u64))
            } else {
                registers.locking_register(target, |registers| {
                    let rhs_target = registers.get_suitable_register(UsageContext::Normal);
                    let compute_rhs =
                        compile_expr(rhs, rhs_target, registers, stack, var_idxs, var_ctx, false);
                    (
                        compute_rhs,
                        Data::Register(rhs_target.as_immutable(BitSize::Bit32)),
                    )
                })
            };
            // now we have lhs in `target` and rhs in `rhs_target`
            match arithmop {
                ArithmeticOp::Add => compute_rhs.chain(registers.using_register_mutably(
                    stack,
                    target,
                    BitSize::Bit32,
                    |_stack, _regs, target| Instruction::Add {
                        target,
                        lhs: target.into(),
                        rhs: rhs_data,
                    },
                )),
                ArithmeticOp::Subtract => compute_rhs.chain(registers.using_register_mutably(
                    stack,
                    target,
                    BitSize::Bit32,
                    |_stack, _regs, target| Instruction::Sub {
                        target,
                        lhs: target.into(),
                        rhs: rhs_data,
                    },
                )),
                ArithmeticOp::Multiply => compute_rhs.chain(registers.using_register_mutably(
                    stack,
                    target,
                    BitSize::Bit32,
                    |_stack, _regs, target| Instruction::Sub {
                        target,
                        lhs: target.into(),
                        rhs: rhs_data,
                    },
                )),
                ArithmeticOp::Divide => compute_rhs.chain(registers.using_register_mutably(
                    stack,
                    target,
                    BitSize::Bit32,
                    |_stack, _regs, target| Instruction::Div {
                        target,
                        lhs: target.into(),
                        rhs: rhs_data,
                        signed: true,
                    },
                )),
                // NOTE: here I have a use for a `get_suitable_register` function that accepts a
                // "locked" range (apart from the already existing).
                ArithmeticOp::Modulo => {
                    compute_rhs.chain(registers.locking_register(target, |registers| {
                        let helper = if let Data::Register(ImmutableRegister(reg)) = rhs_data {
                            registers.locking_register(
                                reg.as_register_descriptor().unwrap(),
                                |registers| {
                                    // this will be the quotient register, that I'm going to throw
                                    registers.get_suitable_register(UsageContext::Normal)
                                },
                            )
                        } else {
                            registers.get_suitable_register(UsageContext::Normal)
                        };
                        // udiv helper, target (lhs), rhs_target
                        // msub target, helper, rhs_target, target (lhs)
                        registers
                            .using_register_mutably(
                                stack,
                                helper,
                                BitSize::Bit32,
                                |_stack, _registers, helper| Instruction::Div {
                                    signed: false,
                                    target: helper,
                                    lhs: target.as_immutable(BitSize::Bit32),
                                    rhs: rhs_data,
                                },
                            )
                            .chain(registers.using_register_mutably(
                                stack,
                                target,
                                BitSize::Bit32,
                                |stack, registers, target| {
                                    let (rhs_reg, put_rhs_in_reg) =
                                        if let Data::Register(ImmutableRegister(reg)) = rhs_data {
                                            (
                                                reg.as_register_descriptor().unwrap(),
                                                AssemblyOutput::new(),
                                            )
                                        } else {
                                            let reg = registers
                                                .get_suitable_register(UsageContext::Normal);
                                            (
                                                reg,
                                                registers.using_register_mutably(
                                                    stack,
                                                    reg,
                                                    BitSize::Bit32,
                                                    |_stack, _registers, reg| Instruction::Mov {
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
            compile_expr(rhs, target, registers, stack, var_idxs, var_ctx, true)
        },
    )
}
