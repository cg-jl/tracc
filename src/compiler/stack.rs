use super::{AssemblyOutput, Compile, CompileWith};
use crate::assembly::*;

pub struct StackManager {
    allocated_size: usize,
    currently_used: usize,
}

pub fn with_stack<F>(cont: F) -> AssemblyOutput
where
    F: Fn(&mut StackManager) -> AssemblyOutput,
{
    let mut stack = StackManager::new();
    let out = cont(&mut stack);
    stack.finalize(out)
}

impl StackManager {
    pub const fn new() -> Self {
        Self {
            allocated_size: 0,
            currently_used: 0,
        }
    }

    pub fn with_alloc_bytes<F, T>(&mut self, amount: usize, mut cont: F) -> T
    where
        F: FnMut(&mut Self, Memory) -> T,
    {
        let target_used = amount + self.currently_used;
        let last_used = self.currently_used;
        self.allocated_size = self.allocated_size.max(target_used);
        self.currently_used = target_used;
        let result = cont(
            self,
            Memory {
                register: Register::StackPointer,
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
        let final_allocated_size = align_to_stack(self.allocated_size);
        let adjust_offset = Assembly::map_instruction(Instruction::map_over_memory(|mem| {
            if let Memory {
                offset: Offset::Undetermined(offt),
                ..
            } = mem
            {
                mem.offset = Offset::Determined(final_allocated_size - *offt);
            }
        }));

        for asm in instructions.iter_mut() {
            adjust_offset(asm);
        }

        if final_allocated_size != 0 {
            instructions.cons_instruction(Instruction::Sub {
                target: Register::StackPointer,
                lhs: Register::StackPointer,
                rhs: Data::Immediate(final_allocated_size as u64),
            });
            instructions.push_instruction(Instruction::Add {
                target: Register::StackPointer,
                lhs: Register::StackPointer,
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
