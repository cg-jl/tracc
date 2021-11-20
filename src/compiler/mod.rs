mod block;
mod expr;
mod function;
mod labels;
use std::iter::FromIterator;
mod program;
pub mod registers;
pub mod stack;
use crate::assembly::*;
use std::collections::vec_deque::IterMut;
use std::collections::VecDeque;
mod target;

pub trait Compile {
    fn compile(self) -> AssemblyOutput;
}

pub trait CompileWith<State> {
    fn compile(self, helper: &mut State) -> AssemblyOutput;
}

#[repr(transparent)]
#[derive(Debug, Clone)]
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
    pub fn extend<T: Into<Assembly>>(&mut self, other: impl IntoIterator<Item = T>) {
        self.inner.extend(other.into_iter().map(|x| x.into()));
    }

    pub fn chain<T: Into<Assembly>>(mut self, other: impl IntoIterator<Item = T>) -> Self {
        self.extend(other);
        self
    }

    pub fn chain_single(mut self, other: impl Into<Assembly>) -> Self {
        self.push_asm(other.into());
        self
    }
    // TODO: join all `singleton`s into a single one
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

impl<A: Into<Assembly>> From<A> for AssemblyOutput {
    fn from(asm: A) -> Self {
        Self::singleton_asm(asm.into())
    }
}

impl IntoIterator for AssemblyOutput {
    type IntoIter = <VecDeque<Assembly> as IntoIterator>::IntoIter;
    type Item = <Self::IntoIter as Iterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<A: Into<Assembly>> FromIterator<A> for AssemblyOutput {
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        Self {
            inner: VecDeque::from_iter(iter.into_iter().map(|x| x.into())),
        }
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
