pub mod assembly;
pub mod flags;
pub mod has_binding;
use crate::allocators::{memory, registers};
use crate::intermediate::*;

use std::collections::VecDeque;

pub struct AssemblyOutput(VecDeque<assembly::Assembly>);

impl AssemblyOutput {
    pub fn new() -> Self {
        Self(VecDeque::new())
    }

    pub fn cons(mut self, value: impl Into<assembly::Assembly>) -> Self {
        self.0.push_front(value.into());
        self
    }

    pub fn push(mut self, value: impl Into<assembly::Assembly>) -> Self {
        self.0.push_back(value.into());
        self
    }

    pub fn extend<T>(mut self, values: impl IntoIterator<Item = T>) -> Self
    where
        T: Into<assembly::Assembly>,
    {
        self.0.extend(values.into_iter().map(T::into));
        self
    }
}

impl IntoIterator for AssemblyOutput {
    type Item = assembly::Assembly;
    type IntoIter = <VecDeque<assembly::Assembly> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> From<T> for AssemblyOutput
where
    T: Into<assembly::Assembly>,
{
    fn from(item: T) -> Self {
        Self::new().cons(item)
    }
}

pub struct BasicBlockLabel(usize);

impl Into<assembly::Assembly> for BasicBlockLabel {
    fn into(self) -> assembly::Assembly {
        assembly::Assembly::Label(format!(".LBB{}", self.0))
    }
}

pub fn codegen_function(function_name: String, ir: IR) -> AssemblyOutput {
    let collisions = crate::intermediate::analysis::compute_lifetime_collisions(&ir);
    // TODO: integrate register spill output
    let register_info = registers::alloc_registers(
        &collisions,
        collisions.keys().cloned().collect(),
        registers::make_allocator_hints(&ir),
    );
    let (memory, mem_size) =
        memory::figure_out_allocations(&ir, memory::make_alloc_map(&ir.code), &collisions);

    let mut output = AssemblyOutput::new()
        // declare function as global for linkage
        .push(assembly::Directive::Global(function_name.clone()))
        .push(assembly::Directive::Type(
            function_name.clone(),
            "function".into(),
        ))
        .push(assembly::Assembly::Label(function_name));

    for (block_index, block) in ir.code.into_iter().enumerate() {
        output = output
            .push(BasicBlockLabel(block_index))
            .extend(compile_block(block, &memory, &register_info.registers))
    }

    if mem_size != 0 {
        // add the proper instructions for setting up the stack.
        // TODO: callee-saved register presaving
        output
            .cons(assembly::Instruction::Sub {
                target: assembly::Register::StackPointer,
                lhs: assembly::Register::StackPointer,
                rhs: assembly::Data::immediate(mem_size as u64, assembly::BitSize::Bit64),
            })
            .push(assembly::Instruction::Add {
                target: assembly::Register::StackPointer,
                lhs: assembly::Register::StackPointer,
                rhs: assembly::Data::immediate(mem_size as u64, assembly::BitSize::Bit64),
            })
    } else {
        output
    }
}

fn compile_block(
    block: BasicBlock,
    memory: &memory::MemoryMap,
    registers: &registers::RegisterMap,
) -> AssemblyOutput {
    let mut output = AssemblyOutput::new();
    for statement in block.statements {
        match statement {
            Statement::Assign { index, value } => {
                let register = registers[&index];
                output = output.extend(compile_value(value, register, memory, registers));
            }
            Statement::Store {
                mem_binding,
                binding,
                byte_size,
            } => todo!(),
        }
    }
    match block.end {
        BlockEnd::Branch(_) => todo!(),
        // TODO: make sure that the returned binding is in the place it should.
        BlockEnd::Return(ret) => output = output.push(assembly::Instruction::Ret),
    }
    output
}

// NOTE: currently the size is always bit32 but there might be a moment in time
// where it's not.
fn could_be_constant_to_data(
    cbc: CouldBeConstant,
    registers: &registers::RegisterMap,
) -> assembly::Data {
    match cbc {
        CouldBeConstant::Binding(binding) => assembly::Data::Register(assembly::Register::from_id(
            registers[&binding],
            assembly::BitSize::Bit32,
        )),
        CouldBeConstant::Constant(constant) => {
            assembly::Data::immediate(constant as u64, assembly::BitSize::Bit32)
        }
    }
}

fn compile_value(
    value: Value,
    target_register: assembly::RegisterID,
    memory: &memory::MemoryMap,
    registers: &registers::RegisterMap,
) -> AssemblyOutput {
    match value {
        Value::Allocate { size } => todo!(),
        Value::Phi { nodes } => todo!(),
        Value::Cmp {
            condition,
            lhs,
            rhs,
        } => AssemblyOutput::from(assembly::Instruction::Cmp {
            register: assembly::Register::from_id(registers[&lhs], assembly::BitSize::Bit32),
            data: could_be_constant_to_data(rhs, registers),
        })
        .push(assembly::Instruction::Cset {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            condition,
        }),
        Value::Load {
            mem_binding,
            byte_size,
        } => todo!(),
        Value::Negate { binding } => assembly::Instruction::Neg {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            source: assembly::Register::from_id(registers[&binding], assembly::BitSize::Bit32),
        }
        .into(),
        // binding XOR FFFFFFFF does the trick.
        Value::FlipBits { binding } => assembly::Instruction::Eor {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            lhs: assembly::Register::from_id(registers[&binding], assembly::BitSize::Bit32),
            rhs: assembly::Data::Immediate(std::u32::MAX as u64),
            bitmask: std::u32::MAX as u64,
        }
        .into(),
        Value::Add { lhs, rhs } => {
            // currently both are only 32 bit
            let lhs_register = registers[&lhs];
            assembly::Instruction::Add {
                target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
                lhs: assembly::Register::from_id(lhs_register, assembly::BitSize::Bit32),
                rhs: could_be_constant_to_data(rhs, registers),
            }
            .into()
        }
        Value::Subtract { lhs, rhs } => assembly::Instruction::Sub {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            lhs: assembly::Register::from_id(registers[&lhs], assembly::BitSize::Bit32),
            rhs: could_be_constant_to_data(rhs, registers),
        }
        .into(),
        Value::Multiply { lhs, rhs } => assembly::Instruction::Mul {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            lhs: assembly::Register::from_id(registers[&lhs], assembly::BitSize::Bit32),
            rhs: could_be_constant_to_data(rhs, registers),
        }
        .into(),
        Value::Divide {
            lhs,
            rhs,
            is_signed,
        } => assembly::Instruction::Div {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            lhs: assembly::Register::from_id(registers[&lhs], assembly::BitSize::Bit32),
            rhs: could_be_constant_to_data(rhs, registers),
            signed: is_signed,
        }
        .into(),
        Value::Lsl { lhs, rhs } => todo!(),
        Value::Lsr { lhs, rhs } => todo!(),
        Value::And { lhs, rhs } => todo!(),
        Value::Or { lhs, rhs } => todo!(),
        Value::Xor { lhs, rhs } => assembly::Instruction::Eor {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            lhs: assembly::Register::from_id(registers[&lhs], assembly::BitSize::Bit32),
            rhs: could_be_constant_to_data(rhs, registers),
            bitmask: std::u32::MAX as u64,
        }
        .into(),
        Value::Constant(ctant) => assembly::Instruction::Mov {
            target: assembly::Register::from_id(
                target_register,
                assembly::BitSize::Bit32, // NOTE: the constant is 32 bit.
            ),
            source: assembly::Data::immediate(ctant as u64, assembly::BitSize::Bit32),
        }
        .into(),
        Value::Binding(_) => todo!(),
    }
}
