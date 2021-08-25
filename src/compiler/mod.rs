mod expr;
mod function;
mod labels;
mod program;
pub mod registers;
pub mod stack;
use crate::assembly::*;
use std::collections::vec_deque::IterMut;
use std::collections::VecDeque;
pub trait Compile {
    fn compile(self) -> AssemblyOutput;
}

pub trait CompileWith<State> {
    fn compile(self, helper: &mut State) -> AssemblyOutput;
}

#[repr(transparent)]
pub struct AssemblyOutput {
    inner: VecDeque<Assembly>,
}

impl Default for AssemblyOutput {
    fn default() -> Self {
        Self::new()
    }
}

impl AssemblyOutput {
    #[inline]
    pub fn push_asm(&mut self, asm: Assembly) {
        self.inner.push_back(asm);
    }
    #[inline]
    pub fn cons_asm(&mut self, asm: Assembly) {
        self.inner.push_front(asm);
    }
    #[inline]
    pub fn push_instruction(&mut self, instruction: Instruction) {
        self.push_asm(Assembly::Instruction(instruction))
    }
    #[inline]
    pub fn cons_instruction(&mut self, instruction: Instruction) {
        self.cons_asm(Assembly::Instruction(instruction))
    }
    #[inline]
    pub fn push_directive(&mut self, directive: Directive) {
        self.push_asm(Assembly::Directive(directive))
    }
    #[inline]
    pub fn cons_directive(&mut self, directive: Directive) {
        self.cons_asm(Assembly::Directive(directive))
    }
    pub fn iter_mut(&mut self) -> IterMut<Assembly> {
        self.inner.iter_mut()
    }
    pub fn new() -> Self {
        Self {
            inner: VecDeque::new(),
        }
    }
    #[inline]
    pub fn release(&mut self) -> std::collections::vec_deque::Drain<'_, Assembly> {
        self.inner.drain(..)
    }
    pub fn extend(&mut self, mut other: Self) {
        self.inner.extend(other.release());
    }
    pub fn singleton_asm(assembly: Assembly) -> Self {
        let mut s = Self::new();
        s.push_asm(assembly);
        s
    }
    pub fn singleton_instruction(instruction: Instruction) -> Self {
        Self::singleton_asm(Assembly::Instruction(instruction))
    }
    pub fn labelled(mut self, label: Label) -> Self {
        self.cons_asm(Assembly::Label(label.to_string()));
        self
    }
    pub fn push_label(&mut self, label: Label) {
        self.push_asm(Assembly::Label(label.to_string()))
    }
}

#[macro_export]
macro_rules! asm_out {
    [] => { $crate::compiler::AssemblyOutput::new() };
    {} => { $crate::compiler::AssemblyOutput::new() };
    {$($expr:expr),+} => { {
        let mut out = $crate::compiler::AssemblyOutput::new();
        $(
            out.push_asm($expr);
        ),+
        out
    } };
    [$($expr:expr),+] => { {
        let mut out = $crate::compiler::AssemblyOutput::new();
        $(
            out.push_instruction($expr);
        ),+
        out
    } };
}
