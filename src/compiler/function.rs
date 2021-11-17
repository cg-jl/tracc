use super::block::compile_block;
use super::registers::with_registers;
use super::registers::RegisterDescriptor;
use super::stack::with_stack;
use super::AssemblyOutput;
use super::Compile;
use crate::assembly::{Assembly, Directive, Instruction};
use crate::ast::{Function, Identifier};

impl Compile for Function {
    fn compile(self) -> AssemblyOutput {
        let mut output = AssemblyOutput::new();
        let Function { name, body } = self;
        let Identifier(name) = name;
        output.push_directive(Directive::Global(name.clone()));
        output.push_directive(Directive::Type(name.clone(), "function".to_string()));
        output.push_asm(Assembly::Label(name));
        output.extend(with_stack(|stack| {
            with_registers(stack, |stack, registers| {
                // UNSAFE: safe, the register 0 is callee-saved
                let r0 = unsafe { RegisterDescriptor::from_index(0) };
                compile_block(stack, registers, &body, r0)
            })
        }));
        output.push_instruction(Instruction::Ret);
        output
    }
}
