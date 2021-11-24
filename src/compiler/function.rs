use super::block::compile_block;
use super::expr::CompileExprError;
use super::registers::with_registers;
use super::registers::RegisterDescriptor;
use super::stack::with_stack;
use super::AssemblyOutput;
use super::CouldCompile;
use crate::assembly::BitSize;
use crate::assembly::{Assembly, Directive, Instruction};
use crate::ast::{Function, Identifier};
use crate::compiler::target::Target;
use crate::variables::walk_block;
use crate::variables::VarError;
use std::fmt;

#[derive(Debug)]
pub enum CompileFunctiorError {
    Var(VarError),
    Expr(CompileExprError),
}

impl std::error::Error for CompileFunctiorError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(match self {
            CompileFunctiorError::Var(var) => var,
            CompileFunctiorError::Expr(expr) => expr,
        })
    }
}

impl fmt::Display for CompileFunctiorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileFunctiorError::Var(var) => var.fmt(f),
            CompileFunctiorError::Expr(expr) => expr.fmt(f),
        }
    }
}

impl From<VarError> for CompileFunctiorError {
    fn from(v: VarError) -> Self {
        Self::Var(v)
    }
}

impl From<CompileExprError> for CompileFunctiorError {
    fn from(e: CompileExprError) -> Self {
        Self::Expr(e)
    }
}

impl CouldCompile for Function {
    type Error = CompileFunctiorError;
    fn could_compile(self) -> Result<AssemblyOutput, Self::Error> {
        let mut output = AssemblyOutput::new();
        let Function { name, body } = self;
        let Identifier(name) = name;
        let is_main = name == "main";
        let (body, var_amt) = walk_block(body)?;
        output.push_directive(Directive::Global(name.clone()));
        output.push_directive(Directive::Type(name.clone(), "function".to_string()));
        output.push_asm(Assembly::Label(name));
        output.extend(with_stack(|stack| {
            // register all the variables in the stack
            stack.with_alloc_bytes(var_amt * 4, |stack, memory| {
                let variables: Vec<_> = memory.partition(4).skip(1).take(var_amt).collect();
                // UNSAFE: safe, the register 0 is callee-saved
                let r0 = unsafe { RegisterDescriptor::from_index(0) };
                with_registers(stack, |stack, registers| {
                    let target = Target::Register {
                        rd: r0,
                        bits: BitSize::Bit32,
                    };
                    // if body is empty (no returns) and it is main then just return 0.
                    if body.0.is_empty() && is_main {
                        Ok(target.load_immediate(0, registers, stack))
                    } else {
                        compile_block(stack, registers, &body, &target, &variables)
                    }
                })
            })
        })?);
        output.push_instruction(Instruction::Ret);
        Ok(output)
    }
}
