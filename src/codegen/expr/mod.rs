mod arithmetic;
mod bit;
mod logic;

use super::{
    assembly::{
        BitSize, Condition, Data, HasBitSize, ImmutableRegister, Instruction, MutableRegister,
    },
    hlir::Expr,
    load_immediate,
    registers::{RegisterDescriptor, RegisterManager, UsageContext},
    stack::StackManager,
    AssemblyOutput, CompileWith, CompilerContext, Memory,
};
use crate::ast::{BinaryOp, UnaryOp};

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
fn compile_as_pointer(expr: Expr, ctx: &mut CompilerContext) -> (Memory, BitSize, AssemblyOutput) {
    match expr {
        Expr::Variable { index } => (ctx.var_ctx[index], BitSize::Bit32, AssemblyOutput::new()),
        _ => {
            unreachable!("no other expressions than variables should be available as pointers yet")
        }
    }
}
fn compile_expr_as_data<'stack>(ctx: &mut CompilerContext, expr: Expr) -> (AssemblyOutput, Data) {
    if let Expr::Constant(b) = expr {
        (
            AssemblyOutput::new(),
            Data::immediate(b as u64, BitSize::Bit32),
        )
    } else {
        ctx.locking_register(ctx.target, move |ctx| {
            let target = ctx.registers().get_suitable_register(UsageContext::Normal);
            let compute_rhs = ctx.with_target(target, |ctx| expr.compile(ctx));
            (
                compute_rhs,
                Data::Register(target.as_immutable(BitSize::Bit32)),
            )
        })
    }
}

fn locking_memory_register<T>(
    Memory {
        register: ImmutableRegister(register),
        offset: _,
    }: Memory,
    ctx: &mut CompilerContext,
    cont: impl FnOnce(&mut CompilerContext) -> T,
) -> T {
    // inspect the register and pass through if nothing
    if let Some(rd) = register.as_register_descriptor() {
        ctx.locking_register(rd, cont)
    } else {
        cont(ctx)
    }
}

impl CompileWith<CompilerContext> for Expr {
    fn compile(self, ctx: &mut CompilerContext) -> AssemblyOutput {
        match self {
            Expr::AlreadyInTarget => AssemblyOutput::new(),
            Expr::Variable { index } => {
                let mem = ctx.var_ctx[index];
                // bit32 as all variables are currently bit32
                ctx.using_register_mutably(ctx.target, BitSize::Bit32, |_ctx, target| {
                    Instruction::Ldr {
                        register: target,
                        address: mem,
                    }
                })
            }
            Expr::Constant(value) => ctx.empty_on_ignore(|ctx| {
                load_immediate(&mut ctx.stack, &mut ctx.registers, ctx.target, value)
            }),
            Expr::Binary {
                operator,
                lhs,
                rhs,
                branch_depends_on_result,
            } => match operator {
                BinaryOp::Arithmetic(arithm) => {
                    arithmetic::compile_arithmetic_op(ctx, arithm, *lhs, *rhs)
                }
                BinaryOp::Bit(bitop) => bit::compile_bit_op(bitop, ctx, *lhs, *rhs),
                BinaryOp::Logic(logicop) => logic::compile_logic_op(ctx, logicop, *lhs, *rhs),
                BinaryOp::Relational(relation) => {
                    // compute both, compare and cset. Simple, right?
                    let compute_lhs = lhs.compile(ctx);
                    let (compute_rhs, rhs_target) = ctx.locking_register(ctx.target, |ctx| {
                        let rhs_target =
                            ctx.registers().get_suitable_register(UsageContext::Normal);
                        let compute_rhs = rhs.compile(ctx);

                        (compute_rhs, rhs_target)
                    });
                    compute_lhs
                        .chain(compute_rhs)
                        .chain_single(Instruction::Cmp {
                            register: ctx.target.as_immutable(BitSize::Bit32),
                            data: Data::Register(rhs_target.as_immutable(BitSize::Bit32)),
                        })
                        .chain(if !branch_depends_on_result {
                            ctx.using_register_mutably(
                                ctx.target,
                                BitSize::Bit32,
                                |_ctx, target| Instruction::Cset {
                                    target,
                                    condition: relation.to_condition(),
                                },
                            )
                        } else {
                            AssemblyOutput::new()
                        })
                }
                BinaryOp::Assignment { op } => {
                    if let Some(assignment_enabled) = op {
                        // if there's a binary operation to do:
                        //   #1 Evaluate the pointer to some place, call it M
                        //   #2 Get a new register that doesn't touch M and inside that bubble
                        //   compile the binary operation using a dummy as the lhs
                        //   #3 Put that register back in the memory controlled by `target`
                        let (target_mem, target_bitsize, target_build) =
                            compile_as_pointer(*lhs, ctx);
                        let (produce_value, value_target) =
                            locking_memory_register(target_mem, ctx, |ctx| {
                                let value_target =
                                    ctx.registers().get_suitable_register(UsageContext::Normal);
                                let get_value_down = ctx.using_register_mutably(
                                    value_target,
                                    target_bitsize,
                                    |_ctx, target| Instruction::Ldr {
                                        register: target,
                                        address: target_mem,
                                    },
                                );
                                let produce_value = Expr::Binary {
                                    operator: assignment_enabled.into(),
                                    lhs: Box::new(Expr::AlreadyInTarget),
                                    rhs,
                                    branch_depends_on_result: false,
                                }
                                .compile(ctx);

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
                        ctx.with_ignoring(false, |ctx| rhs.compile(ctx)).chain(
                            ctx.locking_register(ctx.target, |ctx| {
                                let (target_mem, target_bitsize, build_target) =
                                    compile_as_pointer(*lhs, ctx);
                                build_target.chain_single(Instruction::Str {
                                    register: ctx.target.as_immutable(target_bitsize),
                                    address: target_mem,
                                })
                            }),
                        )
                    }
                }
            },
            Expr::Unary { operator, inner } => {
                let expr = inner.compile(ctx);
                expr.chain(ctx.empty_on_ignore(|ctx| {
                    // // NOTE: bit32 as everything is an int
                    ctx.using_register_mutably(ctx.target, BitSize::Bit32, |_ctx, target| {
                        compile_unary(operator, target)
                    })
                }))
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
