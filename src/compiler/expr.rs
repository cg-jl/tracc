use super::labels::LabelGenerator;
use super::registers::RegisterDescriptor;
use super::registers::RegisterManager;
use super::registers::UsageContext;
use super::stack::StackManager;
use super::AssemblyOutput;
use super::Memory;
use crate::assembly::Offset;
use crate::assembly::{BitSize, Branch, Condition, Data, Instruction, Register};
use crate::ast::BinaryOp;
use crate::ast::Expr;
use crate::ast::UnaryOp;
use crate::ast::VariableKind;

// TODO: remove unused expressions
pub fn compile_expr(
    expr: &Expr,
    target: RegisterDescriptor,
    registers: &mut RegisterManager,
    stack: &mut StackManager,
    is_lvalue: bool,
    var_ctx: &[Memory],
) -> AssemblyOutput {
    match expr {
        Expr::Variable(VariableKind::Unprocessed(name)) => {
            unreachable!(&format!("unprocessed variable: {}", name))
        }
        Expr::Variable(VariableKind::Processed { index }) => {
            if is_lvalue {
                let Memory { register, offset } = var_ctx[*index];
                if let Offset::Undetermined(offt) = offset {
                    registers.using_register(stack, target, BitSize::Bit64, |_, _, target| {
                        AssemblyOutput::singleton_instruction(Instruction::Add {
                            target,
                            lhs: Register::StackPointer,
                            rhs: Data::StackOffset(offt as u64),
                        })
                    })
                } else {
                    unreachable!()
                }
            } else {
                registers.using_register(stack, target, BitSize::Bit64, |_, _, target| {
                    AssemblyOutput::singleton_instruction(Instruction::Ldr {
                        register: target,
                        address: var_ctx[*index],
                    })
                })
            }
        }
        Expr::Constant(value) => {
            registers.using_register(stack, target, BitSize::Bit64, |_, _, target| {
                let mut output = AssemblyOutput::new();
                output.push_instruction(Instruction::Mov {
                    target,
                    source: Data::immediate(*value as u64, BitSize::Bit64),
                });
                output
            })
        }
        Expr::Unary { operator, expr } => {
            let mut expr_out = compile_expr(expr, target, registers, stack, false, var_ctx);
            expr_out.extend(registers.using_register(
                stack,
                target,
                BitSize::Bit64,
                |_, _, target| compile_unary(*operator, target),
            ));
            expr_out
        }
        Expr::Binary {
            operator: BinaryOp::LogicAnd,
            lhs,
            rhs,
        } => {
            // binaryop_logicand rx:
            //   <rx <- lhs>
            //   cmp rx, #0
            //   beq fail
            //   <rx <- rhs>
            //   cmp rx, #0
            //   beq fail
            //   mov rx, #1
            //   b   end
            // fail:
            //   mov rx, #0
            // end:
            let (failed, end) = {
                let gen = LabelGenerator::global();
                (gen.new_label(), gen.new_label())
            };
            registers.using_register(
                stack,
                target,
                BitSize::Bit64,
                |stack, registers, register| {
                    let failed_out = AssemblyOutput::singleton_instruction(Instruction::Mov {
                        target: register,
                        source: Data::Immediate(0),
                    })
                    .labelled(failed);
                    let ok = Instruction::Mov {
                        target: register,
                        source: Data::Immediate(1),
                    };
                    let mut out = compile_expr(lhs, target, registers, stack, false, var_ctx);
                    // if register is zero, bail out
                    out.push_instruction(Instruction::Cmp {
                        register,
                        data: Data::Immediate(0),
                    });
                    out.push_instruction(Instruction::Branch(Branch::Conditional {
                        condition: Condition::Equals,
                        label: failed,
                    }));
                    // compute second one
                    out.extend(compile_expr(rhs, target, registers, stack, false, var_ctx));
                    // if register is zero, bail out
                    out.push_instruction(Instruction::Cmp {
                        register,
                        data: Data::Immediate(0),
                    });
                    out.push_instruction(Instruction::Branch(Branch::Conditional {
                        condition: Condition::Equals,
                        label: failed,
                    }));
                    // otherwise, move the 1 and terminate
                    out.push_instruction(ok);
                    out.push_instruction(Instruction::Branch(Branch::Unconditional {
                        register: None,
                        label: end,
                    }));
                    out.extend(failed_out);
                    out.push_label(end);

                    out
                },
            )
        }
        Expr::Binary {
            operator: BinaryOp::LogicOr,
            lhs,
            rhs,
        } => {
            // binary_logicor rx:
            //   <lhs->rx>
            //   cmp rx, #0
            //   bne set_one
            //   <rhs->rx>
            //   cmp rx, #0
            //   beq set_zero
            // set_one:
            //   mov rx, #1
            //   b   end
            // set_zero:
            //   mov rx, #0
            // end:
            let (set_one, set_zero, end) = {
                let gen = LabelGenerator::global();
                (gen.new_label(), gen.new_label(), gen.new_label())
            };
            registers.using_register(
                stack,
                target,
                BitSize::Bit64,
                |stack, registers, register| {
                    // compute lhs
                    let mut out = compile_expr(lhs, target, registers, stack, false, var_ctx);
                    // if lhs != 0, then go to shortcut
                    out.push_instruction(Instruction::Cmp {
                        register,
                        data: Data::Immediate(0),
                    });
                    out.push_instruction(Instruction::Branch(Branch::Conditional {
                        condition: Condition::NotEquals,
                        label: set_one,
                    }));
                    // compute rhs
                    out.extend(compile_expr(rhs, target, registers, stack, false, var_ctx));
                    // if rhs is zero, go to set_zero
                    out.push_instruction(Instruction::Cmp {
                        register,
                        data: Data::Immediate(0),
                    });
                    out.push_instruction(Instruction::Branch(Branch::Conditional {
                        condition: Condition::Equals,
                        label: set_zero,
                    }));

                    // set_one:
                    //    mov <rx>, #1
                    //    b   end
                    out.push_label(set_one);
                    out.push_instruction(Instruction::Mov {
                        target: register,
                        source: Data::Immediate(1),
                    });
                    out.push_instruction(Instruction::Branch(Branch::Unconditional {
                        register: None,
                        label: end,
                    }));
                    // set_zero:
                    //    mov <rx>, #0
                    out.push_label(set_zero);
                    out.push_instruction(Instruction::Mov {
                        target: register,
                        source: Data::Immediate(0),
                    });
                    // end:
                    out.push_label(end);

                    out
                },
            )
        }
        Expr::Binary { operator, lhs, rhs } => {
            // we *want* to evaluate lhs first, then rhs
            // get a different register than `target`
            let (lhs_target, mut lhs_out) = registers.locking_register(target, |registers| {
                let target = registers.get_suitable_register(UsageContext::Normal);
                let out = compile_expr(
                    lhs,
                    target,
                    registers,
                    stack,
                    *operator == BinaryOp::Assign,
                    var_ctx,
                );
                (target, out)
            });
            let rhs_out = registers.locking_register(lhs_target, |registers| {
                compile_expr(rhs, target, registers, stack, false, var_ctx)
            });
            lhs_out.extend(rhs_out);
            lhs_out.extend(registers.using_register(
                stack,
                target,
                BitSize::Bit64,
                |stack, registers, rhs| {
                    registers.using_register(stack, lhs_target, BitSize::Bit64, |_, _, lhs| {
                        compile_binary(*operator, lhs, rhs, rhs)
                    })
                },
            ));
            lhs_out
        }
    }
}
fn compile_binary(op: BinaryOp, lhs: Register, rhs: Register, target: Register) -> AssemblyOutput {
    let mut output = AssemblyOutput::new();
    match op {
        BinaryOp::Assign => output.push_instruction(Instruction::Str {
            register: rhs,
            address: Memory {
                register: lhs,
                offset: Offset::Determined(0),
            },
        }),
        BinaryOp::Add => output.push_instruction(Instruction::Add {
            target,
            lhs,
            rhs: Data::Register(rhs),
        }),
        BinaryOp::Subtract => output.push_instruction(Instruction::Sub {
            target,
            lhs,
            rhs: Data::Register(rhs),
        }),
        BinaryOp::Multiply => output.push_instruction(Instruction::Mul {
            target,
            lhs,
            rhs: Data::Register(rhs),
        }),
        BinaryOp::Divide => output.push_instruction(Instruction::Div {
            target,
            lhs,
            rhs: Data::Register(rhs),
            signed: true,
        }),
        BinaryOp::Relational(rel) => {
            output.push_instruction(Instruction::Cmp {
                register: lhs,
                data: Data::Register(rhs),
            });
            output.push_instruction(Instruction::Cset {
                target,
                condition: rel.to_condition(),
            });
        }
        BinaryOp::Equality(eq) => {
            output.push_instruction(Instruction::Cmp {
                register: lhs,
                data: Data::Register(rhs),
            });
            output.push_instruction(Instruction::Cset {
                target,
                condition: eq.to_condition(),
            });
        }
        BinaryOp::LogicAnd | BinaryOp::LogicOr => {
            unreachable!("`&&` and `||` are handled separately")
        }
    }
    output
}

fn into_bool(register: Register, output: &mut AssemblyOutput, expect_zero: bool) {
    let condition = if expect_zero {
        Condition::Equals
    } else {
        Condition::NotEquals
    };
    output.push_instruction(Instruction::Cmp {
        register,
        data: Data::immediate(0, register.bit_size()),
    });
    output.push_instruction(Instruction::Cset {
        target: register,
        condition,
    });
}

fn compile_unary(op: UnaryOp, target: Register) -> AssemblyOutput {
    let mut output = AssemblyOutput::new();
    match op {
        UnaryOp::BitNot => output.push_instruction(Instruction::MvN {
            target,
            source: Data::Register(target),
        }),
        UnaryOp::LogicNot => into_bool(target, &mut output, true),
        UnaryOp::Negate => output.push_instruction(Instruction::Neg {
            target,
            source: target,
        }),
    }
    output
}
