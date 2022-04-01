use crate::{
    ast::BitOp,
    codegen::{
        assembly::{BitSize, Instruction},
        hlir::Expr,
        AssemblyOutput, CompileWith, CompilerContext,
    },
};

use super::compile_expr_as_data;

pub fn compile_bit_op(
    bitop: BitOp,
    ctx: &mut CompilerContext,
    lhs: Expr,
    rhs: Expr,
) -> AssemblyOutput {
    // kind of same stuff as arithmetic operations
    lhs.compile(ctx)
        .chain(ctx.checking_ignored(|ctx, is_ignored| {
            if !is_ignored {
                let (compute_rhs, rhs_data) = compile_expr_as_data(ctx, rhs);
                compute_rhs.chain(ctx.using_register_mutably(
                    ctx.target,
                    BitSize::Bit32,
                    |_ctx, target| match bitop {
                        BitOp::And => Instruction::And {
                            target,
                            lhs: target.into(),
                            rhs: rhs_data,
                        },
                        BitOp::Or => Instruction::Orr {
                            target,
                            lhs: target.into(),
                            rhs: rhs_data,
                        },
                        BitOp::Xor => Instruction::Eor {
                            target,
                            lhs: target.into(),
                            rhs: rhs_data,
                            bitmask: BitSize::Bit32.full_bits(),
                        },
                        BitOp::LeftShift => Instruction::Lsl {
                            target,
                            lhs: target.into(),
                            rhs: rhs_data,
                        },
                        BitOp::RightShift => Instruction::Lsr {
                            target,
                            lhs: target.into(),
                            rhs: rhs_data,
                        },
                    },
                ))
            } else {
                ctx.with_ignoring(true, |ctx| rhs.compile(ctx))
            }
        }))
}
