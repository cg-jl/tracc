use super::assembly::MutableRegister;
use super::assembly::{Assembly, Directive, Instruction};
use super::hlir::Function;
use super::registers::RegisterDescriptor;
use super::AssemblyOutput;
use super::Compile;
use super::CompileWith;
use super::CompilerContext;

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
        let mut output = {
            // UNSAFE: safe, no registers are being used right now. And we have to put our return
            // there.
            let r0 = unsafe { RegisterDescriptor::from_index(0) };
            let mut ctx = CompilerContext::new(r0, var_amt);
            let output = body.compile(&mut ctx);
            ctx.finalize(output)
        };
        // if main hasn't had a return yet, add it.
        if is_main
            && !matches!(
                output.last(),
                Some(&Assembly::Instruction(Instruction::Ret))
            )
        {
            // return zero.
            output.push_instruction(Instruction::Mov {
                target: MutableRegister(super::assembly::Register::GeneralPurpose {
                    index: 0,
                    bit_size: super::assembly::BitSize::Bit32,
                }),
                source: super::assembly::Data::immediate(0, super::assembly::BitSize::Bit32),
            });
            output.push_instruction(Instruction::Ret);
        }

        output
    }
}
