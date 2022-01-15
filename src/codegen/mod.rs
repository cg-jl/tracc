pub mod assembly;
mod block;
mod constant_folding;
mod expr;
mod function;
pub mod hlir;
mod labels;
mod program;
mod registers;
mod stack;
use super::output::Output;
use crate::output_impl_From;
use assembly::*;
pub use registers::RegisterDescriptor;

pub fn load_immediate(
    stack: &mut stack::StackManager,
    registers: &mut registers::RegisterManager,
    target: registers::RegisterDescriptor,
    value: i32,
) -> AssemblyOutput {
    registers.using_register_mutably(stack, target, BitSize::Bit32, |_, _, target| {
        Instruction::Mov {
            target,
            source: Data::Immediate(value as u64),
        }
    })
}

pub trait Compile {
    fn compile(self) -> AssemblyOutput;
}

pub trait CompileWith<State> {
    fn compile(self, helper: &mut State) -> AssemblyOutput;
}

pub type AssemblyOutput = Output<Assembly>;
output_impl_From!(Assembly);
