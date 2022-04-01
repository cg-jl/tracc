use super::assembly::Directive;
use super::hlir::Program;

use super::AssemblyOutput;
use super::Compile;

impl Compile for Program<'_> {
    fn compile(self) -> AssemblyOutput {
        let Program(functions) = self;
        let mut out = AssemblyOutput::new();

        for function in functions {
            out.extend(function.compile());
        }

        out.cons_directive(Directive::Architecture(String::from("armv8-a")));
        out
    }
}
