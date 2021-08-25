use super::{stack::StackManager, AssemblyOutput, CompileWith};
use crate::assembly::*;
use std::collections::{HashMap, HashSet};

// TODO: differentiate between mutable and non-mutable access in
// both instructions and the register manager

pub fn with_registers<F>(stack: &mut StackManager, mut cont: F) -> AssemblyOutput
where
    F: FnMut(&mut StackManager, &mut RegisterManager) -> AssemblyOutput,
{
    let mut registers = RegisterManager::new();
    let out = cont(stack, &mut registers);
    registers.finalize(stack, out)
}

fn saving_register<F>(stack: &mut StackManager, register: Register, mut cont: F) -> AssemblyOutput
where
    F: FnMut(&mut StackManager) -> AssemblyOutput,
{
    stack.with_alloc_bytes(8, |stack, address| {
        let mut output = cont(stack);
        output.cons_instruction(Instruction::Str { register, address });
        output.push_instruction(Instruction::Ldr { register, address });
        output
    })
}

fn saving_registers<F>(
    stack: &mut StackManager,
    registers: &[Register],
    mut cont: F,
) -> AssemblyOutput
where
    F: FnMut() -> AssemblyOutput,
{
    let reg_amt = registers.len();
    let size = reg_amt * 8;
    stack.with_alloc_bytes(size, |_stack, addr| {
        let mut output = cont();
        let addresses = addr.partition(8).take(reg_amt);
        for (register, address) in registers.iter().copied().zip(addresses) {
            output.cons_instruction(Instruction::Str { register, address });
            output.push_instruction(Instruction::Ldr { register, address });
        }
        output
    })
}

pub struct RegisterManager {
    register_usage: HashMap<u8, usize>,
    presaved_registers: HashSet<u8>,
}

impl RegisterManager {
    pub fn new() -> Self {
        Self {
            register_usage: HashMap::new(),
            presaved_registers: HashSet::new(),
        }
    }
    fn find_unused_register(&self, range: std::ops::RangeInclusive<u8>) -> Option<u8> {
        for x in range {
            if self.register_usage.get(&x).filter(|x| **x != 0).is_none() {
                return Some(x);
            }
        }
        None
    }
    fn find_min_usage_register(&self, range: std::ops::RangeInclusive<u8>) -> Option<u8> {
        self.register_usage
            .iter()
            .filter(|(k, _)| range.contains(k))
            .min_by(|(_, usesa), (_, usesb)| usesa.cmp(usesb))
            .map(|(id, _)| *id)
    }
    fn find_register(&self, range: std::ops::RangeInclusive<u8>) -> Option<u8> {
        self.find_unused_register(range.clone())
            .or_else(|| self.find_min_usage_register(range))
    }
    pub fn get_suitable_register(&self, usage_ctx: UsageContext) -> RegisterDescriptor {
        let register = match usage_ctx {
            UsageContext::Normal => self
                .find_unused_register(0..=7)
                .or_else(|| self.find_unused_register(9..=15))
                .or_else(|| self.find_min_usage_register(0..=7))
                .or_else(|| self.find_min_usage_register(9..=15))
                .unwrap(),
            UsageContext::AcrossFunction => self.find_register(19..=28).unwrap(),
        };
        RegisterDescriptor { register }
    }
    pub fn using_registers<F>(
        &mut self,
        stack: &mut StackManager,
        register_data: &[(RegisterDescriptor, BitSize)],
        mut cont: F,
    ) -> AssemblyOutput
    where
        F: FnMut(&[Register]) -> AssemblyOutput,
    {
        let registers: Vec<_> = register_data
            .iter()
            .copied()
            .map(|(reg, bs)| reg.real_register(bs))
            .collect();
        let registers_need_saving: Vec<_> = register_data
            .iter()
            .copied()
            .filter_map(|(reg, bs)| {
                let RegisterDescriptor { register } = reg;
                if *self.register_usage.entry(register).or_insert(0) != 0 {
                    Some(reg.real_register(bs))
                } else {
                    None
                }
            })
            .collect();

        saving_registers(stack, &registers_need_saving, || cont(&registers))
    }
    pub fn using_register<F>(
        &mut self,
        stack: &mut StackManager,
        register: RegisterDescriptor,
        bit_size: BitSize,
        cont: F,
    ) -> AssemblyOutput
    where
        F: Fn(&mut StackManager, &mut Self, Register) -> AssemblyOutput,
    {
        let RegisterDescriptor { register } = register;
        if (9..=15).contains(&register) {
            self.presaved_registers.insert(register);
        }
        let real_register = RegisterDescriptor { register }.real_register(bit_size);
        let current = *self.register_usage.entry(register).or_insert(0);
        if current != 0 {
            saving_register(stack, real_register, |stack| {
                cont(stack, self, real_register)
            })
        } else {
            cont(stack, self, real_register)
        }
    }
    pub fn locking_register<F, T>(&mut self, register: RegisterDescriptor, mut cont: F) -> T
    where
        F: FnMut(&mut Self) -> T,
    {
        let RegisterDescriptor { register } = register;
        *self.register_usage.entry(register).or_insert(0) += 1;
        let result = cont(self);
        self.register_usage.entry(register).and_modify(|x| *x -= 1);
        result
    }
    pub fn finalize(
        mut self,
        stack: &mut StackManager,
        mut output: AssemblyOutput,
    ) -> AssemblyOutput {
        let total_registers = self.presaved_registers.len();
        let needed_size = total_registers * 8;

        stack.with_alloc_bytes(needed_size, |_stack, start| {
            let registers = self
                .presaved_registers
                .drain()
                .map(|register| RegisterDescriptor { register }.real_register(BitSize::Bit64));
            let addresses = start.partition(8).take(total_registers);
            for (register, address) in registers.zip(addresses) {
                output.cons_instruction(Instruction::Str { register, address });
                output.push_instruction(Instruction::Ldr { register, address });
            }
        });
        output
    }
}

impl Default for RegisterManager {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> CompileWith<StackManager> for T
where
    T: for<'a> CompileWith<(RegisterManager, &'a mut StackManager)>,
{
    fn compile(self, stack: &mut StackManager) -> AssemblyOutput {
        let mut state = (RegisterManager::new(), stack);
        let out = self.compile(&mut state);
        state.0.finalize(state.1, out)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UsageContext {
    Normal,
    AcrossFunction,
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct RegisterDescriptor {
    register: u8,
}

impl RegisterDescriptor {
    const fn real_register(self, bit_size: BitSize) -> Register {
        Register::GeneralPurpose {
            index: self.register,
            bit_size,
        }
    }
    /// Obtain a register descriptor from an index
    ///
    /// # Safety
    /// This function doesn't check whether the register is
    /// valid to use or not, or if it is even in the `0..=30` range
    pub const unsafe fn from_index(index: u8) -> Self {
        Self { register: index }
    }
}
