use super::registers::RegisterDescriptor;
use super::registers::RegisterManager;
use super::registers::UsageContext;
use super::stack::StackManager;
use super::AssemblyOutput;
use crate::assembly::{BitSize, Condition, Data, Instruction, Register};
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
        Expr::Binary { operator, lhs, rhs } => {
            let lhs_target = registers.locking_register(target, |registers| {
                registers.get_suitable_register(UsageContext::Normal)
            });
            let mut lhs_out = compile_expr(lhs, lhs_target, registers, stack);
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
        BinaryOp::LogicAnd => {
            into_bool()
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
