use super::block::compile_block;
use super::load_immediate;
use super::registers::with_registers;
use super::registers::RegisterDescriptor;
use super::stack::with_stack;
use super::AssemblyOutput;
use super::Compile;
use crate::assembly::{Assembly, Directive, Instruction};
use crate::ast::{Function, Identifier};
use crate::variables::walk_block;

impl Compile for Function<'_> {
    fn compile(self) -> AssemblyOutput {
        let mut output = AssemblyOutput::new();
        let Function { name, body } = self;
        let Identifier(name) = name;
        let is_main = name == "main";
        // NOTE: walking should be done before compilation phase, not during it
        let (body, var_amt) = walk_block(body).unwrap();
        output.push_directive(Directive::Global(name.to_string()));
        output.push_directive(Directive::Type(name.to_string(), "function".to_string()));
        output.push_asm(Assembly::Label(name.to_string()));
        output.extend(with_stack(move |stack| {
            // register all the variables in the stack
            stack.with_alloc_bytes(var_amt * 4, move |stack, memory| {
                let mut variables: Vec<_> = memory.partition(4).skip(1).take(var_amt).collect();
                variables.reverse();
                // UNSAFE: safe, the register 0 is callee-saved
                let r0 = unsafe { RegisterDescriptor::from_index(0) };
                with_registers(stack, move |stack, registers| {
                    // if body is empty (no returns) and it is main then just return 0.
                    if body.0.is_empty() && is_main {
                        load_immediate(stack, registers, r0, 0)
                    } else {
                        compile_block(stack, registers, body, r0, &variables)
                    }
                })
            })
        }));
        output.push_instruction(Instruction::Ret);
        output
    }
}
