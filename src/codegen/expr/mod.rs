mod arithmetic;
mod bit;
mod logic;

use std::collections::HashMap;

use super::{
    assembly::{
        BitSize, Condition, Data, HasBitSize, ImmutableRegister, Instruction, MutableRegister,
    },
    load_immediate,
    registers::{RegisterDescriptor, RegisterManager, UsageContext},
    stack::StackManager,
    AssemblyOutput, Memory,
};
use crate::{
    ast::{ArithmeticOp, BinaryOp, BitOp, Expr, LogicOp, OpFlags, UnaryOp},
    error::Span,
    grammar::lexer::Source,
};

// TODO: make a span-free AST (call it codegen-ready AST) and convert from Program<'source> to
// CodegenFunction(s)

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
// NOTE: this might need a way to lock (mutable) registers used in `Memory` places
// when more expression types are available as pointers
fn compile_as_pointer<'source>(
    expr: Expr,
    _stack: &mut StackManager,
    _registers: &mut RegisterManager,
    var_idxs: &HashMap<&'source str, usize>,
    var_ctx: &[Memory],
) -> (Memory, BitSize, AssemblyOutput) {
    match expr {
        Expr::Variable {
            name: Source { source: name, .. },
        } => (
            var_ctx[var_idxs[name]],
            BitSize::Bit32,
            AssemblyOutput::new(),
        ),
        _ => {
            unreachable!("no other expressions than variables should be available as pointers yet")
        }
    }
}

fn locking_memory_register<T>(
    Memory {
        register: ImmutableRegister(register),
        offset: _,
    }: Memory,
    registers: &mut RegisterManager,
    cont: impl FnOnce(&mut RegisterManager) -> T,
) -> T {
    // inspect the register and pass through if nothing
    if let Some(rd) = register.as_register_descriptor() {
        registers.locking_register(rd, cont)
    } else {
        cont(registers)
    }
}

// compile expression to a register
pub fn compile_expr<'source>(
    expr: Expr<'source>,
    target: RegisterDescriptor,
    registers: &mut RegisterManager,
    stack: &mut StackManager,
    var_idxs: &HashMap<&'source str, usize>,
    var_ctx: &[Memory],
    is_ignored: bool,
) -> AssemblyOutput {
    match reduce_expr(expr) {
        Expr::AlreadyInTarget => AssemblyOutput::new(),
        Expr::Variable {
            name: Source { source: name, .. },
        } => {
            let mem = var_ctx[var_idxs[name]];
            // bit32 as all variables are currently bit32
            registers.using_register_mutably(
                stack,
                target,
                BitSize::Bit32,
                |_stack, _registers, target| Instruction::Ldr {
                    register: target,
                    address: mem,
                },
            )
        }
        Expr::Constant(value) => {
            if !is_ignored {
                load_immediate(stack, registers, target, value)
            } else {
                AssemblyOutput::new()
            }
        }
        Expr::Binary {
            operator,
            lhs: (lhs, _),
            rhs: (rhs, rhs_span),
        } => match operator {
            BinaryOp::Arithmetic(arithm) => arithmetic::compile_arithmetic_op(
                arithm, target, *lhs, *rhs, registers, stack, var_idxs, var_ctx, is_ignored,
            ),
            BinaryOp::Bit(bitop) => bit::compile_bit_op(
                bitop, target, *lhs, *rhs, registers, stack, var_idxs, var_ctx, is_ignored,
            ),
            BinaryOp::Logic(logicop) => logic::compile_logic_op(
                logicop, target, *lhs, *rhs, registers, stack, var_idxs, var_ctx, is_ignored,
            ),
            BinaryOp::Relational(relation) => {
                // compute both, compare and cset. Simple, right?
                let compute_lhs = compile_expr(
                    *lhs, target, registers, stack, var_idxs, var_ctx, is_ignored,
                );
                let (compute_rhs, rhs_target) = registers.locking_register(target, |registers| {
                    let rhs_target = registers.get_suitable_register(UsageContext::Normal);
                    let compute_rhs = compile_expr(
                        *rhs, rhs_target, registers, stack, var_idxs, var_ctx, is_ignored,
                    );

                    (compute_rhs, rhs_target)
                });
                compute_lhs
                    .chain(compute_rhs)
                    .chain_single(Instruction::Cmp {
                        register: target.as_immutable(BitSize::Bit32),
                        data: Data::Register(rhs_target.as_immutable(BitSize::Bit32)),
                    })
                    .chain(registers.using_register_mutably(
                        stack,
                        target,
                        BitSize::Bit32,
                        |_stack, _registers, target| Instruction::Cset {
                            target,
                            condition: relation.to_condition(),
                        },
                    ))
            }
            BinaryOp::Assignment { op } => {
                if let Some(assignment_enabled) = op {
                    // if there's a binary operation to do:
                    //   #1 Evaluate the pointer to some place, call it M
                    //   #2 Get a new register that doesn't touch M and inside that bubble
                    //   compile the binary operation using a dummy as the lhs
                    //   #3 Put that register back in the memory controlled by `target`
                    let (target_mem, target_bitsize, target_build) =
                        compile_as_pointer(*lhs, stack, registers, var_idxs, var_ctx);
                    let (produce_value, value_target) =
                        locking_memory_register(target_mem, registers, |registers| {
                            let value_target =
                                registers.get_suitable_register(UsageContext::Normal);
                            let get_value_down = registers.using_register_mutably(
                                stack,
                                value_target,
                                target_bitsize,
                                |_stack, _regs, target| Instruction::Ldr {
                                    register: target,
                                    address: target_mem,
                                },
                            );
                            let produce_value = compile_expr(
                                Expr::Binary {
                                    operator: assignment_enabled.into(),
                                    lhs: (Box::new(Expr::AlreadyInTarget), Span::new(0)),
                                    rhs: (rhs, rhs_span),
                                },
                                value_target,
                                registers,
                                stack,
                                var_idxs,
                                var_ctx,
                                false, // not ignored as it will be put into the memory
                            );

                            (get_value_down.chain(produce_value), value_target)
                        });
                    target_build
                        .chain(produce_value)
                        .chain_single(Instruction::Str {
                            register: value_target.as_immutable(target_bitsize),
                            address: target_mem,
                        })
                } else {
                    // this one is easier. just compile rhs to `target` and then put it into the value
                    compile_expr(*rhs, target, registers, stack, var_idxs, var_ctx, false).chain(
                        registers.locking_register(target, |registers| {
                            let (target_mem, target_bitsize, build_target) =
                                compile_as_pointer(*lhs, stack, registers, var_idxs, var_ctx);
                            build_target.chain_single(Instruction::Str {
                                register: target.as_immutable(target_bitsize),
                                address: target_mem,
                            })
                        }),
                    )
                }
            }
        },
        Expr::Unary {
            operator,
            expr: (expr, _),
        } => {
            let expr = compile_expr(
                *expr, target, registers, stack, var_idxs, var_ctx, is_ignored,
            );
            if is_ignored {
                expr
            } else {
                // NOTE: bit32 as everything is an int
                expr.chain(registers.using_register_mutably(
                    stack,
                    target,
                    BitSize::Bit32,
                    |_, _, target| compile_unary(operator, target),
                ))
            }
        }
    }
}
// fn compile_logic(
//     op: LogicOp,
//     lhs: Expr,
//     rhs: Expr,
//     registers: &mut RegisterManager,
//     stack: &mut StackManager,
// ) -> AssemblyOutput {
//     match op {
//         LogicOp::And => {}
//     }
// }

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

// TODO: wrap all the stack, registers, var_ctx and flags in a compiler context

/// Reorder binary expressions using commutative/associative laws so that all the constants end up
/// together (with maybe other binary expressions) and on the right side
fn reorder_binary_expr<'source>(
    op: impl OpFlags + PartialEq<BinaryOp> + Copy,
    lhs: Expr<'source>,
    rhs: Expr<'source>,
) -> (Expr<'source>, Expr<'source>) {
    match (reduce_expr(lhs), reduce_expr(rhs)) {
        // we're looking at a case like (a o c) o (b o d).
        // We can do the following optimizations:
        (
            Expr::Binary {
                operator: op1,
                lhs: (a, _),
                rhs: (b, _),
            },
            Expr::Binary {
                operator: op2,
                lhs: (c, _),
                rhs: (d, _),
            },
        ) if op == op1 && op == op2 => {
            let (a, b) = reorder_binary_expr(op, *a, *b);
            let (c, d) = reorder_binary_expr(op, *c, *d);
            match (a, b, c, d) {
                //  - if the expression looks like (a o k1) o (b o k2) and the operator is both commutative
                //    and associative, we can do the following manipulation:
                //      (a o k1) o (b o k2)
                //    = a o (k1 o (b o k2)) ; associativity
                //    = a o (k1 o (k2 o b)) ; commutativity
                //    = a o ((k1 o k2) o b) ; associativity
                //    = a o (b o (k1 o k2)) ; commutativity
                //    = (a o b) o (k1 o k2) ; associativity
                //    and reduce the constant to have a K = k1 o k2
                //    = (a o b) o K
                (a, k1 @ Expr::Constant(_), b, k2 @ Expr::Constant(_))
                    if op.is_associative() && op.is_commutative() =>
                {
                    (
                        Expr::Binary {
                            operator: op1,
                            lhs: (Box::new(a), Span::new(0)),
                            rhs: (Box::new(b), Span::new(0)),
                        },
                        reduce_binary_expr(op1, k1, k2),
                    )
                }
                // - if the expression looks like (a o k1) o (k2 o b) and the operator is
                // associative, we can get it to look like:
                //  (a o k1) o (k2 o b)
                //= a o ((k1 o k2) o b) ; associativity
                // if we name k = k1 o k2, we have
                //= a o (k o b), which is already better than the last one (one less operation,
                // same number of registers)
                // if the operator is commutative too, we can reduce the register count by douing
                // the following operations:
                //  a o (k o b)
                //= a o (b o k) ; commutativity
                //= (a o b) o k ; associativity
                //and we can have 2 operations and 2  registers (instead of 3)
                (a, k1 @ Expr::Constant(_), k2 @ Expr::Constant(_), b) => {
                    let k = reduce_binary_expr(op1, k1, k2);
                    if op.is_commutative() {
                        // (a o b) o k
                        (
                            Expr::Binary {
                                operator: op1,
                                lhs: (Box::new(a), Span::new(0)),
                                rhs: (Box::new(b), Span::new(0)),
                            },
                            k,
                        )
                    } else {
                        // a o (k o b)
                        (
                            a,
                            Expr::Binary {
                                operator: op1,
                                lhs: (Box::new(k), Span::new(0)),
                                rhs: (Box::new(b), Span::new(0)),
                            },
                        )
                    }
                }
                // there are no other ways to improve the code that I can see with this pattern
                (a, b, c, d) => (
                    Expr::Binary {
                        operator: op1,
                        lhs: (Box::new(a), Span::new(0)),
                        rhs: (Box::new(b), Span::new(0)),
                    },
                    Expr::Binary {
                        operator: op1,
                        lhs: (Box::new(c), Span::new(0)),
                        rhs: (Box::new(d), Span::new(0)),
                    },
                ),
            }
        }
        // we are looking at an expression in the form (a o b) o c
        (
            Expr::Binary {
                operator: op1,
                lhs: (a, _),
                rhs: (b, _),
            },
            c,
        ) if op == op1 => {
            let (a, b) = reorder_binary_expr(op, *a, *b);
            match (a, b, c) {
                // if the expression looks like (k1 o a) o k2, we can do the following:
                //  (k1 o a) o k2
                //= k1 o (a o k2) ; associativity
                //= k1 o (k2 o a) ; commutativity
                //= (k1 o k2) o a ; associativity
                //(let k = k1 o k2)
                //= k o a = a o k ; commutativity
                (k1 @ Expr::Constant(_), a, k2 @ Expr::Constant(_))
                    if op.is_commutative() && op.is_associative() =>
                {
                    let k = reduce_binary_expr(op1, k1, k2);
                    (a, k)
                }
                // if the expression looks like (a o k1) o k2, we can do:
                //  (a o k1) o k2
                //= a o (k1 o k2) ; associativity
                // (let k = k1 o k2)
                //= a o k
                (a, k1 @ Expr::Constant(_), k2 @ Expr::Constant(_)) if op.is_associative() => {
                    let k = reduce_binary_expr(op1, k1, k2);
                    (a, k)
                }
                // otherwise, rearranging in any way won't reduce the amount of operations/memory
                // needed
                (a, b, c) => (
                    Expr::Binary {
                        operator: op1,
                        lhs: (Box::new(a), Span::new(0)),
                        rhs: (Box::new(b), Span::new(0)),
                    },
                    c,
                ),
            }
        }

        // we are looking into an expression in the form a o (b o c)
        (
            a,
            Expr::Binary {
                operator: op1,
                lhs: (b, _),
                rhs: (c, _),
            },
        ) if op == op1 => {
            let (b, c) = reorder_binary_expr(op, *b, *c);
            match (a, b, c) {
                // if we have something in the form:
                // k1 o (k2 o a)
                // we can manipulate it:
                //      k1 o (k2 o a)
                //  =   (k1 o k2) o a ; associativity
                // let k = k1 o k2;
                //   = k o a
                // if the operator is commutative we can even do a o k and reduce the memory needed
                (k1 @ Expr::Constant(_), k2 @ Expr::Constant(_), a) if op.is_associative() => {
                    let k = reduce_binary_expr(op1, k1, k2);
                    if op.is_commutative() {
                        (a, k)
                    } else {
                        (k, a)
                    }
                }
                // if we have sometihng in the form:
                // k1 o (a o k2)
                //  = k1 o (k2 o a) ; commutative
                //  = (k1 o k2) o a ; associative
                //  = a o (k1 o k2) ; commutative
                (k1 @ Expr::Constant(_), a, k2 @ Expr::Constant(_))
                    if op.is_commutative() && op.is_associative() =>
                {
                    let k = reduce_binary_expr(op1, k1, k2);
                    (a, k)
                }
                // now we don't have ony constants on the right side, so the only one left
                // is k o (a o b), which we can put on the other side if the operator is
                // associative.
                (k @ Expr::Constant(_), a, b) if op.is_commutative() => (
                    Expr::Binary {
                        operator: op1,
                        lhs: (Box::new(a), Span::new(0)),
                        rhs: (Box::new(b), Span::new(0)),
                    },
                    k,
                ),
                // otherwise, there's really nothing to do.
                (a, b, c) => (
                    a,
                    Expr::Binary {
                        operator: op1,
                        lhs: (Box::new(b), Span::new(0)),
                        rhs: (Box::new(c), Span::new(0)),
                    },
                ),
            }
        }
        // now, if the constant is in the wrong place we can fix it
        (k @ Expr::Constant(_), b) if op.is_commutative() => (b, k),
        // otherwise, we can't do anything
        (a, b) => (a, b),
    }
}

fn reduce_binary_expr<'source>(
    operator: BinaryOp,
    lhs: Expr<'source>,
    rhs: Expr<'source>,
) -> Expr<'source> {
    match (reduce_expr(lhs), reduce_expr(rhs)) {
        // two constants can be reduce further with their operator
        (Expr::Constant(a), Expr::Constant(b)) => Expr::Constant(match operator {
            // don't know if this has been handled yet.
            // TODO(#6): handle lvalue errors before getting into compilation
            BinaryOp::Assignment { .. } => {
                panic!("assign operator can't be between two constants")
            }
            BinaryOp::Bit(bitop) => match bitop {
                BitOp::And => a & b,
                BitOp::Or => a | b,
                BitOp::LeftShift => a.wrapping_shl(b as u32),
                BitOp::RightShift => a.wrapping_shr(b as u32),
                BitOp::Xor => a ^ b,
            },
            BinaryOp::Arithmetic(arithmetic_op) => match arithmetic_op {
                // C-style operations
                ArithmeticOp::Add => a.wrapping_add(b),
                ArithmeticOp::Divide => {
                    // NOTE: I could just return the `Divide` operation to be compiled without
                    // evaluating it if we're on integer division.
                    if b == 0 {
                        panic!("in constant expression: division by zero (`{} / {}`)", a, b)
                    } else {
                        a / b
                    }
                }
                ArithmeticOp::Subtract => a.wrapping_sub(b),
                ArithmeticOp::Multiply => a.wrapping_mul(b),
                ArithmeticOp::Modulo => {
                    if b == 0 {
                        panic!("in constant expression: modulo division by zero")
                    } else {
                        a % b
                    }
                }
            },
            BinaryOp::Logic(logicop) => match logicop {
                LogicOp::And => {
                    if a != 0 && b != 0 {
                        1
                    } else {
                        0
                    }
                }
                LogicOp::Or => {
                    if a != 0 || b != 0 {
                        1
                    } else {
                        0
                    }
                }
            },
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
                crate::ast::Relational::Equals => {
                    if a == b {
                        1
                    } else {
                        0
                    }
                }
                crate::ast::Relational::NotEquals => {
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
            lhs: (Box::new(a), Span::new(0)),
            rhs: (Box::new(b), Span::new(0)),
        },
    }
}

fn reduce_expr(expr: Expr) -> Expr {
    match expr {
        Expr::AlreadyInTarget => expr,
        Expr::Variable { .. } => expr, // cannot reduce a variable lookup
        Expr::Constant(_) => expr,     // cannot reduce a numeric constant further
        Expr::Binary {
            operator,
            lhs: (lhs, _),
            rhs: (rhs, _),
        } => reduce_binary_expr(operator, *lhs, *rhs),
        // again, if reducing the expression results in a number, we'll apply the unary operation,
        // otherwise we'll pack it again
        Expr::Unary {
            operator,
            expr: (expr, _),
        } => match reduce_expr(*expr) {
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
                expr: (Box::new(e), Span::new(0)),
            },
        },
    }
}
