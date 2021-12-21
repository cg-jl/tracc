use super::{
    labels::LabelGenerator,
    load_immediate,
    registers::{RegisterDescriptor, RegisterManager, UsageContext},
    stack::StackManager,
    AssemblyOutput, Memory,
};
use crate::{
    assembly::{
        BitSize, Branch, Condition, Data, HasBitSize, ImmutableRegister, Instruction,
        MutableRegister,
    },
    ast::{BinaryOp, Expr, UnaryOp, VariableKind},
};
use std::fmt;

#[derive(Debug)]
pub enum CompileExprError {
    ExprNotAssignable(String),
}

impl std::error::Error for CompileExprError {}

impl fmt::Display for CompileExprError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExprNotAssignable(expr) => write!(f, "expression is not assignable: {:?}", expr),
        }
    }
}

/// NOTE: when adding pointers, I may need to lock-in the registers used for the pointers

// fn using_memory_location<T: Into<AssemblyOutput>>(
//     stack: &mut StackManager,
//     registers: &mut RegisterManager,
//     loc: Memory,
//     memory_size: BitSize,
//     wanted_register: RegisterDescriptor,
//     output: AssemblyOutput,
//     update_after_use: bool,
// ) -> AssemblyOutput {
//     // load the memory in before modifying it
//     let immutable_use = registers
//         .using_register_mutably(
//             stack,
//             wanted_register,
//             memory_size,
//             |_stack, _regs, target| Instruction::Ldr {
//                 register: target,
//                 address: loc,
//             },
//         )
//         .chain(output);
//     if update_after_use {
//         immutable_use.chain_single(Instruction::Str {
//             register: wanted_register.as_immutable(memory_size),
//             address: loc,
//         })
//     } else {
//         immutable_use // otherwise forget about the existence of the register
//     }
// }

// interpet the expression as a pointer
// XXX: this might need a way to lock (mutable) registers used in `Memory` places
// when more expression types are available as pointers
fn compile_as_pointer(
    expr: Expr,
    _stack: &mut StackManager,
    _registers: &mut RegisterManager,
    var_ctx: &[Memory],
) -> (Memory, BitSize, AssemblyOutput) {
    match expr {
        Expr::Variable(VariableKind::Processed { index }) => {
            (var_ctx[index], BitSize::Bit32, AssemblyOutput::new())
        }
        _ => {
            unreachable!("no other expressions than variables should be available as pointers yet")
        }
    }
}

// compile expression to a register
pub fn compile_expr(
    expr: Expr,
    target: RegisterDescriptor,
    registers: &mut RegisterManager,
    stack: &mut StackManager,
    var_ctx: &[Memory],
    is_ignored: bool,
) -> Result<AssemblyOutput, CompileExprError> {
    match reduce_expr(expr) {
        Expr::Variable(VariableKind::Unprocessed(name)) => {
            unreachable!(&format!("unprocessed variable: {}", name))
        }
        Expr::Variable(VariableKind::Processed { index }) => {
            let mem = var_ctx[index];
            // bit32 as all variables are currently bit32
            Ok(registers.using_register_mutably(
                stack,
                target,
                BitSize::Bit32,
                |_stack, _registers, target| Instruction::Ldr {
                    register: target,
                    address: mem,
                },
            ))
        }
        Expr::Constant(value) => Ok(if !is_ignored {
            load_immediate(stack, registers, target, value)
        } else {
            AssemblyOutput::new()
        }),
        Expr::Unary { operator, expr } => {
            let expr = compile_expr(*expr, target, registers, stack, var_ctx, is_ignored)?;
            Ok(if is_ignored {
                expr
            } else {
                // XXX: bit32 as everything is an int
                expr.chain(registers.using_register_mutably(
                    stack,
                    target,
                    BitSize::Bit32,
                    |_, _, target| compile_unary(operator, target),
                ))
            })
        }
        Expr::Binary {
            operator: BinaryOp::LogicAnd,
            lhs,
            rhs,
        } => {
            let lhs = *lhs;
            let rhs = *rhs;
            match (lhs, rhs) {
                (Expr::Constant(a), b) | (b, Expr::Constant(a)) => {
                    if a == 0 {
                        // don't bother doing the other one
                        Ok(load_immediate(stack, registers, target, 0))
                    } else {
                        compile_expr(b, target, registers, stack, var_ctx, is_ignored)
                    }
                }
                (lhs, rhs) => {
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
                    let (failed, maybe_end) = {
                        let gen = LabelGenerator::global();
                        (
                            gen.new_label(),
                            if !is_ignored {
                                Some(gen.new_label())
                            } else {
                                None
                            },
                        )
                    };
                    let failed_out = if !is_ignored {
                        load_immediate(stack, registers, target, 0)
                    } else {
                        AssemblyOutput::new()
                    }
                    .labelled(failed);
                    let ok = if !is_ignored {
                        load_immediate(stack, registers, target, 1)
                    } else {
                        AssemblyOutput::new()
                    };
                    // first is taken into account to check if the program can skip the second
                    let compute_first =
                        compile_expr(lhs, target, registers, stack, var_ctx, false)?;
                    let compute_second =
                        compile_expr(rhs, target, registers, stack, var_ctx, is_ignored)?;
                    // comparison doesn't modify the location
                    let compare_and_bail = [
                        Instruction::Cmp {
                            register: target.as_immutable(BitSize::Bit32), // XXX: bit32 because only int32 exists yet
                            data: Data::Immediate(0),
                        },
                        Instruction::Branch(Branch::Conditional {
                            condition: Condition::Equals,
                            label: failed,
                        }),
                    ];
                    Ok(compute_first
                        .chain(compare_and_bail.clone())
                        .chain(compute_second)
                        .chain(compare_and_bail)
                        .chain(ok)
                        .chain(maybe_end.map(|end| {
                            Instruction::Branch(Branch::Unconditional {
                                register: None,
                                label: end,
                            })
                        }))
                        .chain(failed_out)
                        .chain(maybe_end))
                }
            }
        }
        Expr::Binary {
            operator: BinaryOp::LogicOr,
            lhs,
            rhs,
        } => {
            let lhs = *lhs;
            let rhs = *rhs;
            match (lhs, rhs) {
                (Expr::Constant(a), b) | (b, Expr::Constant(a)) => {
                    // if  one already succeeds then we don't bother doing the other one
                    if a != 0 {
                        Ok(load_immediate(stack, registers, target, 1))
                    } else {
                        // we bother doing the other one
                        compile_expr(b, target, registers, stack, var_ctx, is_ignored)
                    }
                }
                (lhs, rhs) => {
                    // binary_logicor rx:
                    //   <lhs->rx>
                    //   cmp rx, #0
                    //   bne set_one
                    //   <rhs->rx>
                    //   cmp rx, #0
                    //   bne set_one
                    //   <0->rx>
                    //   b    end
                    // set_one:
                    //   mov rx, #1
                    // end:
                    let (set_one, maybe_end) = {
                        let gen = LabelGenerator::global();
                        (
                            gen.new_label(),
                            if !is_ignored {
                                Some(gen.new_label())
                            } else {
                                None
                            },
                        )
                    };
                    let compare_zero = Instruction::Cmp {
                        register: target.as_immutable(BitSize::Bit32), // XXX: only bit32 is needed due to only int32 existing
                        data: Data::Immediate(0),
                    };
                    // compute lhs
                    Ok(compile_expr(lhs, target, registers, stack, var_ctx, false)?
                        // if lhs != 0, then go to set_one and shortcut
                        .chain_single(compare_zero)
                        .chain_single(Instruction::Branch(Branch::Conditional {
                            condition: Condition::NotEquals,
                            label: set_one,
                        }))
                        // otherwise, compute rhs
                        .chain(compile_expr(
                            rhs, target, registers, stack, var_ctx, is_ignored,
                        )?)
                        // if it wasn't zero, then go to set_one again
                        .chain(if !is_ignored {
                            compare_zero.into()
                        } else {
                            AssemblyOutput::new()
                        })
                        .chain(if !is_ignored {
                            Some(Instruction::Branch(Branch::Conditional {
                                condition: Condition::NotEquals,
                                label: set_one,
                            }))
                        } else {
                            None
                        })
                        // otherwise, load a zero and go to end
                        .chain(if !is_ignored {
                            load_immediate(stack, registers, target, 0)
                        } else {
                            AssemblyOutput::new()
                        })
                        .chain(maybe_end.map(|end| Branch::Unconditional {
                            register: None,
                            label: end,
                        }))
                        // set_one: put one
                        .chain(
                            if !is_ignored {
                                load_immediate(stack, registers, target, 1)
                            } else {
                                AssemblyOutput::new()
                            }
                            .labelled(set_one),
                        )
                        .chain(maybe_end))
                }
            }
        }
        Expr::Binary {
            operator: BinaryOp::Assign,
            lhs,
            rhs,
        } => {
            let (lhs_mem, lhs_bitsize, prepare_lhs) = registers
                .locking_register(target, |registers| {
                    compile_as_pointer(*lhs, stack, registers, var_ctx)
                });
            let prepare_rhs = compile_expr(*rhs, target, registers, stack, var_ctx, false)?;
            Ok(prepare_rhs
                .chain(prepare_lhs)
                .chain_single(Instruction::Str {
                    register: target.as_immutable(lhs_bitsize),
                    address: lhs_mem,
                }))
        }
        Expr::Binary { operator, lhs, rhs } => {
            let lhs = *lhs;
            let rhs = *rhs;
            // NOTE: revise register locking; could do something about it
            // we *want* to evaluate lhs first, then rhs
            // get a different register than `target`
            let (lhs_target, lhs_out) = registers.locking_register(target, |registers| {
                let target = registers.get_suitable_register(UsageContext::Normal);
                let out = compile_expr(lhs, target, registers, stack, var_ctx, is_ignored);
                out.map(|out| (target, out))
            })?;
            let rhs_out = registers.locking_register(lhs_target, |registers| {
                compile_expr(rhs, target, registers, stack, var_ctx, is_ignored)
            })?;
            let binop = if !is_ignored {
                registers.using_register_mutably(stack, target, BitSize::Bit32, |_, _, target| {
                    compile_binary(
                        operator,
                        lhs_target.as_immutable(BitSize::Bit32),
                        target.into(),
                        target,
                    )
                })
            } else {
                AssemblyOutput::new()
            };
            Ok(lhs_out.chain(rhs_out).chain(binop))
        }
    }
}
fn compile_binary(
    op: BinaryOp,
    lhs: ImmutableRegister,
    rhs: ImmutableRegister,
    target: MutableRegister,
) -> AssemblyOutput {
    let mut output = AssemblyOutput::new();
    match op {
        BinaryOp::Assign => unreachable!("must have been implemented in `compile_expr`"),
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

fn into_bool(register: MutableRegister, output: &mut AssemblyOutput, expect_zero: bool) {
    let condition = if expect_zero {
        Condition::Equals
    } else {
        Condition::NotEquals
    };
    output.push_instruction(Instruction::Cmp {
        register: register.into(),
        data: Data::immediate(0, register.get_bit_size()),
    });
    output.push_instruction(Instruction::Cset {
        target: register,
        condition,
    });
}

fn compile_unary(op: UnaryOp, target: MutableRegister) -> AssemblyOutput {
    let mut output = AssemblyOutput::new();
    match op {
        UnaryOp::BitNot => output.push_instruction(Instruction::MvN {
            target,
            source: Data::Register(target.into()),
        }),
        UnaryOp::LogicNot => into_bool(target, &mut output, true),
        UnaryOp::Negate => output.push_instruction(Instruction::Neg {
            target,
            source: target.into(),
        }),
    }
    output
}

fn reduce_expr(expr: Expr) -> Expr {
    match expr {
        Expr::Variable(_) => expr, // cannot reduce a variable lookup
        Expr::Constant(_) => expr, // cannot reduce a numeric constant further
        Expr::Binary { operator, lhs, rhs } => {
            match (reduce_expr(*lhs), reduce_expr(*rhs)) {
                // two constants can be reduce further with their operator
                (Expr::Constant(a), Expr::Constant(b)) => Expr::Constant(match operator {
                    // C-style operations
                    BinaryOp::Add => a.wrapping_add(b),
                    // don't know if this has been handled yet.
                    // TODO(#6): handle lvalue errors before getting into compilation
                    BinaryOp::Assign => panic!("assign operator can't be between two constants"),
                    BinaryOp::Divide => {
                        if b == 0 {
                            // TODO(#7): add span tags on expressions for errors
                            panic!("in constant expression: division by zero (`{} / {}`)", a, b)
                        } else {
                            a / b
                        }
                    }
                    BinaryOp::Subtract => a.wrapping_sub(b),
                    BinaryOp::Multiply => a.wrapping_mul(b),
                    BinaryOp::LogicAnd => {
                        if a != 0 && b != 0 {
                            1
                        } else {
                            0
                        }
                    }
                    BinaryOp::LogicOr => {
                        if a != 0 || b != 0 {
                            1
                        } else {
                            0
                        }
                    }
                    BinaryOp::Relational(relation) => match relation {
                        crate::ast::Relational::Less => {
                            if a < b {
                                1
                            } else {
                                0
                            }
                        }
                        crate::ast::Relational::LessEqual => {
                            if a <= b {
                                1
                            } else {
                                0
                            }
                        }
                        crate::ast::Relational::Greater => {
                            if a > b {
                                1
                            } else {
                                0
                            }
                        }
                        crate::ast::Relational::GreaterEqual => {
                            if a >= b {
                                1
                            } else {
                                0
                            }
                        }
                    },
                    BinaryOp::Equality(eq_constraint) => match eq_constraint {
                        crate::ast::Equality::Equals => {
                            if a == b {
                                1
                            } else {
                                0
                            }
                        }
                        crate::ast::Equality::NotEquals => {
                            if a != b {
                                1
                            } else {
                                0
                            }
                        }
                    },
                }),
                // otherwise, just pack them again
                (a, b) => Expr::Binary {
                    operator,
                    lhs: Box::new(a),
                    rhs: Box::new(b),
                },
            }
        }
        // again, if reducing the expression results in a number, we'll apply the unary operation,
        // otherwise we'll pack it again
        Expr::Unary { operator, expr } => match reduce_expr(*expr) {
            Expr::Constant(c) => Expr::Constant(match operator {
                UnaryOp::Negate => -c,
                UnaryOp::BitNot => !c,
                UnaryOp::LogicNot => {
                    if c == 0 {
                        1
                    } else {
                        0
                    }
                }
            }),
            e => Expr::Unary {
                operator,
                expr: Box::new(e),
            },
        },
    }
}
