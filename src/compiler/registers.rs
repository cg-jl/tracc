use super::{stack::StackManager, AssemblyOutput, CompileWith};
use crate::assembly::*;
use std::ops::Try;

pub fn with_registers<F, O>(stack: &mut StackManager, cont: F) -> O
where
    F: FnOnce(&mut StackManager, &mut RegisterManager) -> O,
    O: Try<Output = AssemblyOutput>,
{
    let mut registers = RegisterManager::new();
    let out = cont(stack, &mut registers)?;
    O::from_output(registers.finalize(stack, out))
}

fn saving_register<F>(
    stack: &mut StackManager,
    register: MutableRegister,
    cont: F,
) -> AssemblyOutput
where
    F: FnOnce(&mut StackManager) -> AssemblyOutput,
{
    stack.with_alloc_bytes(8, move |stack, address| {
        let mut output = cont(stack);
        output.cons_instruction(Instruction::Str {
            register: register.into(),
            address,
        });
        output.push_instruction(Instruction::Ldr { register, address });
        output
    })
}

fn saving_registers<F>(
    stack: &mut StackManager,
    registers: &[MutableRegister],
    cont: F,
) -> AssemblyOutput
where
    F: FnOnce(&mut StackManager) -> AssemblyOutput,
{
    let reg_amt = registers.len();
    let size = reg_amt * 8;
    stack.with_alloc_bytes(size, |stack, addr| {
        let mut output = cont(stack);
        let addresses = addr.partition(8).take(reg_amt);
        for (register, address) in registers.iter().copied().zip(addresses) {
            output.cons_instruction(Instruction::Str {
                register: register.into(),
                address,
            });
            output.push_instruction(Instruction::Ldr { register, address });
        }
        output
    })
}

// presaved registers are 9 to 15 (included), therefore we can use u8

#[derive(Default)]
#[repr(transparent)]
struct PresavedRegisters(u8);

impl PresavedRegisters {
    // raw set with index
    #[inline]
    fn set(&mut self, register_id: u8) {
        let index = register_id - 9;
        self.0 |= 1 << index;
    }
    #[inline]
    const fn get(&self, register_id: u8) -> bool {
        let index = register_id - 9;
        self.0 >> index != 0
    }
}

impl const IntoIterator for PresavedRegisters {
    type Item = u8;

    type IntoIter = PresavedRegistersIter;

    fn into_iter(self) -> Self::IntoIter {
        PresavedRegistersIter(self, 0u8)
    }
}

struct PresavedRegistersIter(PresavedRegisters, u8);

impl Iterator for PresavedRegistersIter {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        while self.1 < 31 {
            if self.0.get(self.1) {
                return Some(self.1);
            }
            self.1 += 1
        }
        None
    }
}

pub struct RegisterManager {
    register_usage: [usize; 31],
    // 31 values? that's a u32
    presaved_registers: PresavedRegisters,
}

impl RegisterManager {
    pub fn new() -> Self {
        Self {
            register_usage: [0usize; 31],
            presaved_registers: PresavedRegisters::default(),
        }
    }
    fn find_unused_register(&self, range: std::ops::RangeInclusive<u8>) -> Option<u8> {
        for x in range {
            if self.register_usage[x as usize] == 0 {
                return Some(x);
            }
        }
        None
    }
    fn find_min_usage_register(&self, range: std::ops::RangeInclusive<u8>) -> Option<u8> {
        self.register_usage[*range.start() as usize..=*range.end() as usize]
            .iter()
            .copied()
            .enumerate()
            .min_by(|(_id_a, usesa), (_id_b, usesb)| usesa.cmp(usesb))
            .map(|(id, _)| id as u8)
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
    /// Given a set of registers, give that set of registers back in a `MutableRegister` so they
    /// can be used in assembly output.
    ///
    /// # Safety
    /// This function has the same caveats that [`using_register_mutably`] but with all of its
    /// registers
    pub fn using_registers_mutably<T: Into<AssemblyOutput>>(
        &mut self,
        stack: &mut StackManager,
        query: &[(RegisterDescriptor, BitSize)],
        user: impl FnOnce(&mut StackManager, &mut Self, &[MutableRegister]) -> T,
    ) -> AssemblyOutput {
        let all_usable_registers: Vec<_> = query
            .iter()
            .copied()
            .map(|(reg, bs)| reg.as_mutable(bs))
            .collect();
        let registers_need_saving: Vec<_> = query
            .iter()
            .copied()
            .filter_map(|(rd, bs)| {
                if self.register_usage[rd.register as usize] != 0 {
                    Some(rd.as_mutable(bs))
                } else {
                    None
                }
            })
            .collect();
        saving_registers(stack, &registers_need_saving, |stack| {
            user(stack, self, &all_usable_registers).into()
        })
    }
    /// Given a function, gives a mutable register to it, saving it if needed.
    ///
    /// # Safety
    /// This currently only works well when the given `MutableRegister` is not moved out of the
    /// called function.
    ///  TODO: make `MutableRegister` not clonable so it prohibits moves out of not allowed spaces
    /// although I don't know if not being able to copy it "just to be safe" is going to affect
    /// anything else.
    pub fn using_register_mutably<T: Into<AssemblyOutput>>(
        &mut self,
        stack: &mut StackManager,
        register: RegisterDescriptor,
        bit_size: BitSize,
        user: impl FnOnce(&mut StackManager, &mut Self, MutableRegister) -> T,
    ) -> AssemblyOutput {
        // first, ensure that the register is saved, if it is one of the callee-saved registers
        {
            let RegisterDescriptor { register } = register;
            if (9..=15).contains(&register) {
                self.presaved_registers.set(register);
            }
        }
        let usable_register = register.as_mutable(bit_size);

        // if (rare case since we've got 9 registers to play with plus presaved ones) it has at least one use, then
        // save it so that the other use can continue, otherwise the register can be used freely.
        let current_uses = self.register_usage[register.register as usize];
        if current_uses != 0 {
            saving_register(stack, usable_register, move |stack| {
                user(stack, self, usable_register).into()
            })
        } else {
            user(stack, self, usable_register).into()
        }
    }
    // TODO: eliminate `locking_register` by adding a set of not wanted registers to `get_suitable_registers`
    pub fn locking_register<F, T>(&mut self, register: RegisterDescriptor, cont: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let RegisterDescriptor { register } = register;
        self.register_usage[register as usize] += 1;
        let result = cont(self);
        self.register_usage[register as usize] -= 1;
        result
    }
    pub fn finalize(self, stack: &mut StackManager, mut output: AssemblyOutput) -> AssemblyOutput {
        let presaved_registers: Vec<_> = self.presaved_registers.into_iter().collect();
        let total_registers = presaved_registers.len();
        let needed_size = total_registers * 8;

        stack.with_alloc_bytes(needed_size, move |_stack, start| {
            // for each of the presaved registers, add a safeguard to make sure that they are saved
            // and later restored
            let registers = presaved_registers
                .into_iter()
                .map(|register| RegisterDescriptor { register }.as_mutable(BitSize::Bit64));
            let addresses = start.partition(8).take(total_registers);
            for (register, address) in registers.zip(addresses) {
                output.cons_instruction(Instruction::Str {
                    register: register.into(),
                    address,
                });
                output.push_instruction(Instruction::Ldr { register, address });
            }
            output
        })
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
    /// Obtain the mutable version of the register. For obvious reasons
    /// it is only accessible by the `RegisterManager` to only give mutable registers to
    #[inline]
    const fn as_mutable(self, bit_size: BitSize) -> MutableRegister {
        MutableRegister(Register::GeneralPurpose {
            index: self.register,
            bit_size,
        })
    }
    /// Obtain the immutable version of the register. This is safe as long as `from_index`
    /// had a valid register index
    #[inline]
    pub const fn as_immutable(self, bit_size: BitSize) -> ImmutableRegister {
        ImmutableRegister(Register::GeneralPurpose {
            index: self.register,
            bit_size,
        })
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
