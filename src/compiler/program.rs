use crate::assembly::Directive;
use crate::ast::Function;
use crate::ast::Program;

use super::AssemblyOutput;
use super::CouldCompile;

impl CouldCompile for Program {
    type Error = <Function as CouldCompile>::Error;
    fn could_compile(self) -> Result<AssemblyOutput, Self::Error> {
        let Program(function) = self;
        let mut out = function.could_compile()?;
        out.cons_directive(Directive::Architecture(String::from("armv8-a")));
        Ok(out)
    }
}
