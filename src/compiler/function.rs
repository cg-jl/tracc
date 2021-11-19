use super::block::compile_block;
use super::registers::with_registers;
use super::registers::RegisterDescriptor;
use super::stack::with_stack;
use super::AssemblyOutput;
use super::Compile;
use crate::assembly::{Assembly, Directive, Instruction};
use crate::ast::{Function, Identifier};
use crate::variables::walk_block;

impl Compile for Function {
    fn compile(self) -> AssemblyOutput {
        let mut output = AssemblyOutput::new();
        let Function { name, mut body } = self;
        let Identifier(name) = name;
        let var_amt = walk_block(&mut body);
        output.push_directive(Directive::Global(name.clone()));
        output.push_directive(Directive::Type(name.clone(), "function".to_string()));
        output.push_asm(Assembly::Label(name));
        output.extend(with_stack(|stack| {
            // register all the variables in the stack
            stack.with_alloc_bytes(var_amt * 4, |stack, memory| {
                // TODO: make another (non-build) context in which each variable is in reach.... probably with some `unsafe`s...
                let variables: Vec<_> = memory.partition(4).take(var_amt).collect();
                dbg!(&variables);
                with_registers(stack, |stack, registers| {
                    // UNSAFE: safe, the register 0 is callee-saved
                    let r0 = unsafe { RegisterDescriptor::from_index(0) };
                    compile_block(stack, registers, &body, r0, &variables)
                })
            })
        }));
        output.push_instruction(Instruction::Ret);
        output
    }
}
