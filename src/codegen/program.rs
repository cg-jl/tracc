use super::assembly::Directive;
use super::hlir::Program;

use super::AssemblyOutput;
use super::Compile;

impl Compile for Program<'_> {
    fn compile(self) -> AssemblyOutput {
        let Program(function) = self;
        let mut out = function.compile();
        out.cons(Directive::Architecture(String::from("armv8-a")));
        out
    }
}
