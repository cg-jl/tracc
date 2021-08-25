use super::AssemblyOutput;
use super::Compile;
use crate::ast::Program;

impl Compile for Program {
    fn compile(self) -> AssemblyOutput {
        let Program(function) = self;
        function.compile()
    }
}
