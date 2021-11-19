use super::expr::compile_expr;
use super::registers::{RegisterDescriptor, RegisterManager};
use super::stack::StackManager;
use super::AssemblyOutput;
use super::BitSize;
use super::Data;
use super::Instruction;
use super::Memory;
use crate::ast::Block;
use crate::ast::Statement;

// NOTE: when I specify mutable and immutable registers; this one has to be mutable
pub fn compile_block(
    stack: &mut StackManager,
    registers: &mut RegisterManager,
    block: &Block,
    bail_return_register: RegisterDescriptor,
    var_ctx: &Vec<Memory>,
) -> AssemblyOutput {
    let mut out = AssemblyOutput::new();
    for statement in &block.0 {
        out.extend(match statement {
            Statement::Return(expr) => {
                compile_expr(expr, bail_return_register, registers, stack, false, var_ctx)
            }
            Statement::DeclareVar(_) => {
                unreachable!("declaring variables should not get to assembly phase")
            }
            Statement::SingleExpr(expr) => {
                // clear the expression after it's done.
                let mut out = compile_expr(expr, bail_return_register, registers, stack, false, var_ctx);

                out.extend(registers.using_register(
                    stack,
                    bail_return_register,
                    BitSize::Bit32,
                    |_, _, target| {
                        AssemblyOutput::singleton_instruction(Instruction::Mov {
                            target,
                            source: Data::Immediate(0),
                        })
                    },
                ));
                out
            }
        })
    }
    out
}
