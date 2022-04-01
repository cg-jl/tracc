pub mod assembly;
mod block;
mod constant_folding;
mod expr;
mod function;
pub mod hlir;
mod labels;
use std::iter::FromIterator;
mod program;
mod registers;
mod stack;
use assembly::*;
pub use registers::RegisterDescriptor;
use std::collections::vec_deque::IterMut;
use std::collections::VecDeque;

use self::registers::RegisterManager;
use self::stack::StackManager;
pub fn load_immediate(
    stack: &mut stack::StackManager,
    registers: &mut registers::RegisterManager,
    target: registers::RegisterDescriptor,
    value: i32,
) -> AssemblyOutput {
    registers.using_register_mutably(stack, target, BitSize::Bit32, |_, _, target| {
        Instruction::Mov {
            target,
            // UNSAFE: safe, they have the same bytes and bits.
            source: Data::immediate(
                unsafe { std::mem::transmute::<i32, u32>(value) } as u64,
                BitSize::Bit32,
            ),
        }
    })
}

#[derive(Copy, Clone)]
pub struct LoopEnv {
    start: Label,
    end: Label,
}

pub struct CompilerContext {
    var_ctx: Vec<Memory>,
    loop_status: Option<LoopEnv>,
    stack: StackManager,
    registers: RegisterManager,
    target: RegisterDescriptor,
    is_current_ignored: bool,
}

impl CompilerContext {
    pub fn registers(&self) -> &RegisterManager {
        &self.registers
    }
    pub fn finalize(self, mut output: AssemblyOutput) -> AssemblyOutput {
        let CompilerContext {
            var_ctx,
            mut stack,
            registers,
            ..
        } = self;

        output = registers.finalize(&mut stack, output);
        // UNSAFE: safe. those bytes were initially allocated in `new` and we've made sure
        // to allocate them the last thing.
        unsafe { stack.dealloc_bytes(var_ctx.len() * 4) };
        stack.finalize(output)
    }
    pub fn using_register_mutably<T: Into<AssemblyOutput>>(
        &mut self,
        descriptor: RegisterDescriptor,
        bit_size: BitSize,
        cont: impl FnOnce(&mut Self, MutableRegister) -> T,
    ) -> AssemblyOutput {
        // UNSAFE: safe, the output returned from the continuation is wrapped from
        // the `end_mutable_use` method.
        let usable_reg = unsafe { self.registers.begin_mutable_use(descriptor, bit_size) };
        let output = cont(self, usable_reg).into();
        unsafe {
            self.registers
                .end_mutable_use(&mut self.stack, usable_reg, output)
        }
    }
    pub fn locking_register<T>(
        &mut self,
        descriptor: RegisterDescriptor,
        cont: impl FnOnce(&mut Self) -> T,
    ) -> T {
        unsafe { self.registers.lock_register(descriptor) };
        let res = cont(self);
        unsafe { self.registers.unlock_register(descriptor) };
        res
    }
    pub fn with_target<T>(
        &mut self,
        target: RegisterDescriptor,
        cont: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let old_target = self.target;
        self.target = target;
        let res = cont(self);
        self.target = old_target;
        res
    }
    pub fn with_ignoring<T>(&mut self, ignoring: bool, cont: impl FnOnce(&mut Self) -> T) -> T {
        let old_ignored = self.is_current_ignored;
        self.is_current_ignored = ignoring;
        let res = cont(self);
        self.is_current_ignored = old_ignored;
        res
    }
    pub fn checking_ignored<T>(&mut self, cont: impl FnOnce(&mut Self, bool) -> T) -> T {
        cont(self, self.is_current_ignored)
    }
    pub fn empty_on_ignore<T: Into<AssemblyOutput>>(
        &mut self,
        cont: impl FnOnce(&mut Self) -> T,
    ) -> AssemblyOutput {
        if self.is_current_ignored {
            AssemblyOutput::new()
        } else {
            cont(self).into()
        }
    }
    pub fn with_loop<T>(&mut self, loop_env: LoopEnv, cont: impl FnOnce(&mut Self) -> T) -> T {
        let old_loop_env = self.loop_status;
        self.loop_status = Some(loop_env);
        let res = cont(self);
        self.loop_status = old_loop_env;
        res
    }
    pub fn new(target: RegisterDescriptor, var_amount: usize) -> Self {
        let mut stack = StackManager::new();
        // UNSAFE: safe, this is deallocated in finalize()
        let mem = unsafe { stack.alloc_bytes(var_amount * 4) };
        Self {
            var_ctx: mem.partition(4).skip(1).take(var_amount).collect(),
            loop_status: None,
            stack,
            registers: RegisterManager::new(),
            target,
            is_current_ignored: false,
        }
    }
    pub fn expect_env(&self) -> LoopEnv {
        self.loop_status
            .expect("should've been inside a loop. check parser")
    }
}

pub trait Compile {
    fn compile(self) -> AssemblyOutput;
}

pub trait CompileWith<State> {
    fn compile(self, ctx: &mut State) -> AssemblyOutput;
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
    pub fn iter(&self) -> impl Iterator<Item = &Assembly> {
        self.inner.iter()
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
    pub fn singleton(item: impl Into<Assembly>) -> Self {
        let mut s = Self::new();
        s.inner.push_back(item.into());
        s
    }
    pub fn labelled(mut self, label: Label) -> Self {
        self.cons_asm(Assembly::Label(label.to_string()));
        self
    }
    pub fn push_label(&mut self, label: Label) {
        self.push_asm(Assembly::Label(label.to_string()))
    }
    pub fn last(&self) -> Option<&Assembly> {
        self.inner.back()
    }
}

impl<A: Into<Assembly>> From<A> for AssemblyOutput {
    fn from(asm: A) -> Self {
        Self::singleton(asm)
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
