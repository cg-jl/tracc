use super::assembly::{Assembly, Directive, Instruction};
use super::ast::Function;
use super::block::compile_block;
use super::load_immediate;
use super::registers::with_registers;
use super::registers::RegisterDescriptor;
use super::stack::with_stack;
use super::AssemblyOutput;
use super::Compile;

impl Compile for Function<'_> {
    fn compile(self) -> AssemblyOutput {
        let mut output = AssemblyOutput::new();
        let Function {
            name,
            body,
            var_amt,
        } = self;
        let is_main = name == "main";
        output.push_directive(Directive::Global(name.to_string()));
        output.push_directive(Directive::Type(name.to_string(), "function".to_string()));
        output.push_asm(Assembly::Label(name.to_string()));
        output.extend(with_stack(move |stack| {
            // register all the variables in the stack
            stack.with_alloc_bytes(var_amt * 4, move |stack, memory| {
                let mut variable_mems: Vec<_> = memory.partition(4).skip(1).take(var_amt).collect();
                variable_mems.reverse();
                // UNSAFE: safe, the register 0 is callee-saved
                let r0 = unsafe { RegisterDescriptor::from_index(0) };
                with_registers(stack, move |stack, registers| {
                    // if body is empty (no returns) and it is main then just return 0.
                    if body.is_empty() && is_main {
                        load_immediate(stack, registers, r0, 0)
                    } else {
                        compile_block(stack, registers, body, r0, &variable_mems)
                    }
                })
            })
        }));
        output.push_instruction(Instruction::Ret);
        output
    }
}
