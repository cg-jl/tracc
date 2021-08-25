use super::labels::LabelGenerator;
use super::registers::RegisterDescriptor;
use super::registers::RegisterManager;
use super::registers::UsageContext;
use super::stack::StackManager;
use super::AssemblyOutput;
use crate::assembly::{BitSize, Branch, Condition, Data, Instruction, Register};
use crate::ast::BinaryOp;
use crate::ast::Expr;
use crate::ast::UnaryOp;

pub fn compile_expr(
    expr: &Expr,
    target: RegisterDescriptor,
    registers: &mut RegisterManager,
    stack: &mut StackManager,
) -> AssemblyOutput {
    match expr {
        Expr::Constant(value) => {
            registers.using_register(stack, target, BitSize::Bit32, |_, _, target| {
                let mut output = AssemblyOutput::new();
                output.push_instruction(Instruction::Mov {
                    target,
                    source: Data::immediate(*value as u64, BitSize::Bit32),
                });
                output
            })
        }
        Expr::Unary { operator, expr } => {
            let mut expr_out = compile_expr(expr, target, registers, stack);
            expr_out.extend(registers.using_register(
                stack,
                target,
                BitSize::Bit32,
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
                BitSize::Bit32,
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
                    let mut out = compile_expr(lhs, target, registers, stack);
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
                    out.extend(compile_expr(rhs, target, registers, stack));
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
                BitSize::Bit32,
                |stack, registers, register| {
                    // compute lhs
                    let mut out = compile_expr(lhs, target, registers, stack);
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
                    out.extend(compile_expr(rhs, target, registers, stack));
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
                let out = compile_expr(lhs, target, registers, stack);
                (target, out)
            });
            let rhs_out = registers.locking_register(lhs_target, |registers| {
                compile_expr(rhs, target, registers, stack)
            });
            lhs_out.extend(rhs_out);
            lhs_out.extend(registers.using_register(
                stack,
                target,
                BitSize::Bit32,
                |stack, registers, rhs| {
                    registers.using_register(stack, lhs_target, BitSize::Bit32, |_, _, lhs| {
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
