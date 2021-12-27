use crate::assembly::Directive;
use crate::ast::Program;

use super::AssemblyOutput;
use super::Compile;

impl Compile for Program<'_> {
    fn compile(self) -> AssemblyOutput {
        let Program(function) = self;
        let mut out = function.compile();
        out.cons_directive(Directive::Architecture(String::from("armv8-a")));
        out
    }
}
