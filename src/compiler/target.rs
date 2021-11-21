use super::{
    registers::{RegisterDescriptor, RegisterManager, UsageContext},
    stack::StackManager,
    AssemblyOutput,
};
use crate::assembly::{BitSize, Data, Instruction, Memory, Register};

#[derive(Debug, Clone)]
pub enum Target {
    Register {
        rd: RegisterDescriptor,
        bits: BitSize,
    },
    Address {
        mem: Memory,
        bits: BitSize,
    },
}

impl Target {
    pub fn put_in_register(
        &self,
        target_register: RegisterDescriptor,
        target_bitsize: BitSize,
        registers: &mut RegisterManager,
        stack: &mut StackManager,
    ) -> AssemblyOutput {
        match self {
            Target::Register { rd, bits } => registers.using_registers(
                stack,
                &[(target_register, target_bitsize), (*rd, *bits)],
                |regs| {
                    let target = regs[0];
                    let current = regs[1];
                    AssemblyOutput::singleton_instruction(Instruction::Mov {
                        target,
                        source: Data::Register(current),
                    })
                },
            ),
            Target::Address { mem, bits } => registers.using_register(
                stack,
                target_register,
                std::cmp::min(*bits, target_bitsize), // we want to not get out of memory bounds
                |_, _, register| {
                    AssemblyOutput::singleton_instruction(Instruction::Ldr {
                        register,
                        address: *mem,
                    })
                },
            ),
        }
    }

    pub fn load_from_target(
        &self,
        target: &Target,
        registers: &mut RegisterManager,
        stack: &mut StackManager,
    ) -> AssemblyOutput {
        match target {
            Target::Register { rd, bits } => self.load_from_register(*rd, *bits, registers, stack),
            Target::Address { mem, bits } => self.load_from_memory(*mem, *bits, registers, stack),
        }
    }

    pub fn load_from_register(
        &self,
        source_register: RegisterDescriptor,
        source_size: BitSize,
        registers: &mut RegisterManager,
        stack: &mut StackManager,
    ) -> AssemblyOutput {
        match self {
            Target::Register { rd, bits } => registers.using_registers(
                stack,
                &[(*rd, *bits), (source_register, source_size)],
                |regs| {
                    let target = regs[0];
                    let source = regs[1];
                    AssemblyOutput::singleton_instruction(Instruction::Mov {
                        source: Data::Register(source),
                        target,
                    })
                },
            ),
            Target::Address { mem, bits } => registers.using_register(
                stack,
                source_register,
                source_size.min(*bits),
                |_, _, source| {
                    AssemblyOutput::singleton_instruction(Instruction::Str {
                        register: source,
                        address: *mem,
                    })
                },
            ),
        }
    }

    // TODO: figure out how to lock memory addresses
    pub fn locking_target<T>(
        &self,
        mut cont: impl FnMut(&mut RegisterManager) -> T,
        registers: &mut RegisterManager,
    ) -> T {
        match self {
            Target::Register { rd, .. } => registers.locking_register(*rd, cont),
            Target::Address { .. } => {
                // NOTE: memory is not locked by the compiler, so a memory target must ensure its
                // TODO: lock register that refers to the memory if it is a general purpose one
                // (writable)
                // own safety.
                cont(registers)
            }
        }
    }

    pub fn through_register<T: Into<AssemblyOutput>>(
        &self,
        cont: impl FnMut(&mut StackManager, &mut RegisterManager, Register) -> T,
        registers: &mut RegisterManager,
        mutable_register: bool,
        stack: &mut StackManager,
    ) -> AssemblyOutput {
        match self {
            Target::Register { rd, bits } => {
                // if we already have a register, then use it!
                registers.using_register(stack, *rd, *bits, cont)
            }
            Target::Address { bits, .. } => {
                // otherwise, get a spare register and use that instead, and then put the stuff
                // back into memory
                let helper = registers.get_suitable_register(UsageContext::Normal);
                let init = registers.using_register(stack, helper, *bits, cont);
                if mutable_register {
                    init.chain(self.load_from_register(helper, *bits, registers, stack))
                } else {
                    init
                }
            }
        }
    }

    pub fn load_immediate(
        &self,
        immediate: i32,
        registers: &mut RegisterManager,
        stack: &mut StackManager,
    ) -> AssemblyOutput {
        let source = Data::Immediate(unsafe { std::mem::transmute(immediate as i64) });
        match self {
            Target::Register { rd, bits } => {
                registers.using_register(stack, *rd, BitSize::Bit32.min(*bits), |_, _, register| {
                    Instruction::Mov {
                        target: register,
                        source,
                    }
                })
            }
            Target::Address { mem, .. } => {
                let helper = registers.get_suitable_register(UsageContext::Normal);
                registers.using_register(stack, helper, BitSize::Bit32, |_, _, target| {
                    AssemblyOutput::from(Instruction::Mov { target, source }).chain_single(
                        Instruction::Str {
                            register: target,
                            address: *mem,
                        },
                    )
                })
            }
        }
    }

    pub fn load_from_memory(
        &self,
        address: Memory,
        address_bitsize: BitSize,
        registers: &mut RegisterManager,
        stack: &mut StackManager,
    ) -> AssemblyOutput {
        match self {
            Target::Register { rd, bits } => registers.using_register(
                stack,
                *rd,
                address_bitsize.min(*bits),
                |_, _, register| Instruction::Ldr { address, register },
            ),
            Target::Address { mem, bits } => {
                let helper = registers.get_suitable_register(UsageContext::Normal);
                let bit_size = address_bitsize.min(*bits);
                registers.using_register(stack, helper, bit_size, |_, _, helper| {
                    AssemblyOutput::new()
                        .chain_single(Instruction::Ldr {
                            register: helper,
                            address,
                        })
                        .chain_single(Instruction::Str {
                            register: helper,
                            address: *mem,
                        })
                })
            }
        }
    }

    // TODO: store byte by byte stuff `BitSize::Byte`
    pub fn put_in_memory(
        &self,
        address: Memory,
        target_bitsize: BitSize,
        registers: &mut RegisterManager,
        stack: &mut StackManager,
    ) -> AssemblyOutput {
        match self {
            Target::Register { rd, bits } => registers.using_register(
                stack,
                *rd,
                std::cmp::min(*bits, target_bitsize),
                |_, _, register| {
                    AssemblyOutput::singleton_instruction(Instruction::Str { register, address })
                },
            ),
            Target::Address { mem, bits } => {
                // we're going to have to use a helper register
                let helper = registers.get_suitable_register(UsageContext::Normal);
                registers.using_register(
                    stack,
                    helper,
                    std::cmp::min(*bits, target_bitsize),
                    |_, _, helper| {
                        let mut out = AssemblyOutput::new();
                        // load the helper register with the value
                        out.push_instruction(Instruction::Ldr {
                            register: helper,
                            address,
                        });
                        // store the contents in the memory
                        out.push_instruction(Instruction::Str {
                            register: helper,
                            address: *mem,
                        });
                        out
                    },
                )
            }
        }
    }

    /// moves the target to a register, invalidating the last position
    pub fn into_register(
        self,
        register: RegisterDescriptor,
        bits: BitSize,
        registers: &mut RegisterManager,
        stack: &mut StackManager,
    ) -> Self {
        self.put_in_register(register, bits, registers, stack);
        Self::Register { rd: register, bits }
    }

    /// moves the target into memory, invalidating the last location
    pub fn into_memory(
        self,
        bits: BitSize,
        address: Memory,
        registers: &mut RegisterManager,
        stack: &mut StackManager,
    ) -> Self {
        self.put_in_memory(address, bits, registers, stack);
        Self::Address { mem: address, bits }
    }
}
