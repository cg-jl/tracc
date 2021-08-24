use crate::ast::Program;
use super::Compile;
use super::AssemblyOutput;
use crate::assembly::{Assembly, Directive, Instruction};

impl Compile for Program {
    fn compile(self) -> AssemblyOutput {
        let Program(function) = self;
        function.compile()
    }
}
