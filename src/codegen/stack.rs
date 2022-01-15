use super::assembly::*;
use super::{AssemblyOutput, Compile, CompileWith};

pub struct StackManager {
    allocated_size: usize,
    currently_used: usize,
}

pub fn with_stack<F, O>(cont: F) -> AssemblyOutput
where
    F: FnOnce(&mut StackManager) -> O,
    O: Into<AssemblyOutput>,
{
    let mut stack = StackManager::new();
    let out = cont(&mut stack);
    stack.finalize(out.into())
}

impl StackManager {
    pub const fn new() -> Self {
        Self {
            allocated_size: 0,
            currently_used: 0,
        }
    }

    pub fn with_alloc_bytes<F, T>(&mut self, amount: usize, cont: F) -> T
    where
        F: FnOnce(&mut Self, Memory) -> T,
    {
        let target_used = amount + self.currently_used;
        let last_used = self.currently_used;
        self.allocated_size = self.allocated_size.max(target_used);
        self.currently_used = target_used;
        let result = cont(
            self,
            Memory {
                register: ImmutableRegister(Register::StackPointer),
                offset: Offset::Undetermined(last_used),
            },
        );
        self.currently_used = last_used;
        result
    }

    pub fn finalize(self, mut instructions: AssemblyOutput) -> AssemblyOutput {
        assert_eq!(
            self.currently_used, 0,
            "finalize() with currently used bytes"
        );
        let final_allocated_size = align_to_stack(self.allocated_size) as u64;
        let adjust_memory = |mem: &mut Memory| {
            if let Offset::Undetermined(from_stack) = mem.offset {
                mem.offset = Offset::Determined(final_allocated_size as usize - from_stack);
            }
        };
        let adjust_data = |data: &mut Data| {
            if let Data::StackOffset(from_stack) = data {
                *data = Data::Immediate(final_allocated_size - *from_stack);
            }
        };
        let adjust_offset = Assembly::map_instruction(|instr| match instr {
            Instruction::Add { rhs, .. } => {
                adjust_data(rhs);
            }
            Instruction::Cmp { data, .. } => adjust_data(data),
            Instruction::Div { rhs, .. } => adjust_data(rhs),
            Instruction::Ldr { address, .. } => adjust_memory(address),
            Instruction::Mov { source, .. } => adjust_data(source),
            Instruction::Mul { rhs, .. } => {
                adjust_data(rhs);
            }
            Instruction::MvN { source, .. } => adjust_data(source),
            Instruction::Str { address, .. } => adjust_memory(address),
            Instruction::Sub { rhs, .. } => {
                adjust_data(rhs);
            }
            // NOTE: this is a good error point
            _ => (),
        });

        for asm in instructions.iter_mut() {
            adjust_offset(asm);
        }

        // SAFETY: this uses directly the registers without asking for permission, but the stack
        // pointer is RESERVED for use in the stack
        if final_allocated_size != 0 {
            instructions.cons(Instruction::Sub {
                target: MutableRegister(Register::StackPointer),
                lhs: ImmutableRegister(Register::StackPointer),
                rhs: Data::Immediate(final_allocated_size as u64),
            });
            instructions.push(Instruction::Add {
                target: MutableRegister(Register::StackPointer),
                lhs: ImmutableRegister(Register::StackPointer),
                rhs: Data::Immediate(final_allocated_size as u64),
            });
        }

        instructions
    }
}

impl<T: CompileWith<StackManager>> Compile for T {
    fn compile(self) -> AssemblyOutput {
        let mut stack = StackManager::default();
        let result = self.compile(&mut stack);
        stack.finalize(result)
    }
}

fn align_to_stack(amount: usize) -> usize {
    if amount == 0 {
        amount
    } else {
        16 * (amount / 16 + 1)
    }
}

impl Default for StackManager {
    fn default() -> Self {
        Self::new()
    }
}
