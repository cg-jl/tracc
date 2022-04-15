pub mod assembly;
pub mod flags;
pub mod has_binding;
use crate::allocators::{memory, registers};
use crate::intermediate::*;

use std::collections::VecDeque;

pub struct AssemblyOutput(VecDeque<assembly::Assembly>);

impl Default for AssemblyOutput {
    fn default() -> Self {
        Self::new()
    }
}

impl AssemblyOutput {
    pub fn new() -> Self {
        Self(VecDeque::new())
    }

    pub fn cons(mut self, value: impl Into<assembly::Assembly>) -> Self {
        self.push_front(value);
        self
    }

    pub fn push_front(&mut self, value: impl Into<assembly::Assembly>) -> &mut Self {
        self.0.push_front(value.into());
        self
    }

    pub fn push_back(&mut self, value: impl Into<assembly::Assembly>) -> &mut Self {
        self.0.push_back(value.into());
        self
    }

    pub fn chain_back<T>(mut self, values: impl IntoIterator<Item = T>) -> Self
    where
        T: Into<assembly::Assembly>,
    {
        for v in values {
            self.0.push_front(v.into());
        }
        self
    }

    pub fn chain_one(mut self, value: impl Into<assembly::Assembly>) -> Self {
        self.push_back(value);
        self
    }

    pub fn chain<T>(mut self, values: impl IntoIterator<Item = T>) -> Self
    where
        T: Into<assembly::Assembly>,
    {
        self.extend(values);
        self
    }

    pub fn extend<T>(&mut self, values: impl IntoIterator<Item = T>) -> &mut Self
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

impl FromIterator<AssemblyOutput> for AssemblyOutput {
    fn from_iter<T: IntoIterator<Item = AssemblyOutput>>(iter: T) -> Self {
        iter.into_iter()
            .fold(AssemblyOutput::new(), AssemblyOutput::chain)
    }
}

impl FromIterator<assembly::Assembly> for AssemblyOutput {
    fn from_iter<T: IntoIterator<Item = assembly::Assembly>>(iter: T) -> Self {
        Self(VecDeque::from_iter(iter))
    }
}

pub struct BasicBlockLabel(usize);

impl From<BasicBlockLabel> for assembly::Assembly {
    fn from(val: BasicBlockLabel) -> Self {
        assembly::Assembly::Label(format!(".LBB{}", val.0))
    }
}

pub fn codegen_function(function_name: String, mut ir: IR) -> AssemblyOutput {
    let collisions = crate::intermediate::analysis::compute_lifetime_collisions(&ir);
    // TODO: integrate register spill output
    let registers::CodegenHints {
        need_move_to_return_reg,
        save_upon_call,
        mut completely_spilled,
        registers,
    } = registers::alloc_registers(
        &ir,
        &collisions,
        analysis::order_by_deps(&ir, collisions.keys().cloned()),
        registers::make_allocator_hints(&ir),
    );

    for binding in registers.iter().filter_map(|(binding, reg)| {
        if matches!(reg, assembly::RegisterID::ZeroRegister) {
            Some(*binding)
        } else {
            None
        }
    }) {
        // UNSAFE: the binding is allocated to a read-only register.
        unsafe { crate::intermediate::refactor::remove_binding(&mut ir, binding) };
    }

    let alloc_map = memory::make_alloc_map(&ir.code);

    alloc_map.keys().cloned().for_each(|allocated_binding| {
        completely_spilled.remove(&allocated_binding);
    });

    debug_assert!(completely_spilled.is_empty(), "shouldn't have any spills");

    let (memory, mem_size) = memory::figure_out_allocations(&ir, alloc_map, &collisions);

    debug_assert!(save_upon_call.is_empty(), "TODO: implement save upon call");

    debug_assert!(
        need_move_to_return_reg.is_empty(),
        "TODO: implement moves to return register (or generic move to register)"
    );

    let r#final: AssemblyOutput = ir
        .code
        .into_iter()
        .enumerate()
        .flat_map(|(block_index, block)| {
            let has_return = matches!(block.end, BlockEnd::Return(_));
            let mut compiled_block = compile_block(block, &memory, &registers);
            if has_return && mem_size != 0 {
                compiled_block.0.insert(
                    compiled_block.0.len() - 1,
                    assembly::Instruction::Add {
                        target: assembly::Register::StackPointer,
                        lhs: assembly::Register::StackPointer,
                        rhs: assembly::Data::Immediate(mem_size as u64),
                    }
                    .into(),
                );
            }

            compiled_block.cons(BasicBlockLabel(block_index))
        })
        .collect();

    r#final
        .chain_back(if mem_size == 0 {
            None
        } else {
            Some(assembly::Instruction::Sub {
                target: assembly::Register::StackPointer,
                lhs: assembly::Register::StackPointer,
                rhs: assembly::Data::Immediate(mem_size as u64),
            })
        })
        // declare function as global for linkage
        .cons(assembly::Assembly::Label(function_name.clone()))
        .cons(assembly::Directive::Type(
            function_name.clone(),
            "function".into(),
        ))
        .cons(assembly::Directive::Global(function_name))
}

fn compile_block(
    block: BasicBlock,
    memory: &memory::MemoryMap,
    registers: &registers::RegisterMap,
) -> AssemblyOutput {
    let mut output = AssemblyOutput::new();
    for statement in block.statements {
        output = output.chain(match statement {
            Statement::Assign { index, value } => {
                let register = registers[&index];
                compile_value(value, register, memory, registers)
            }
            Statement::Store {
                mem_binding,
                binding,
                byte_size,
            } => match byte_size {
                ByteSize::U64 => todo!("64-bit stores"),
                ByteSize::U8 => todo!("one byte stores"),
                ByteSize::U32 => assembly::Instruction::Str {
                    register: assembly::Register::from_id(
                        registers[&binding],
                        assembly::BitSize::Bit32,
                    ),
                    address: memory[&mem_binding],
                }
                .into(),
            },
        });
    }
    match block.end {
        BlockEnd::Branch(branch) => match branch {
            Branch::Unconditional { target } => {
                output = output.chain_one(assembly::Branch::Unconditional {
                    register: None,
                    label: assembly::BasicBlockLabel::new(target.0),
                })
            }
            Branch::Conditional {
                flag,
                target_true,
                target_false,
            } => {
                // TODO: map with known already touched CPU flags at the end of every block, and
                // the result they computed, if any.
                output = output
                    .chain_one(assembly::Instruction::Cmp {
                        register: assembly::Register::from_id(
                            registers[&flag],
                            assembly::BitSize::Bit32,
                        ),
                        data: assembly::Data::immediate(0, assembly::BitSize::Bit32),
                    })
                    .chain_one(assembly::Branch::Conditional {
                        condition: assembly::Condition::Equals,
                        label: assembly::BasicBlockLabel::new(target_false.0),
                    })
                    .chain_one(assembly::Branch::Unconditional {
                        register: None,
                        label: assembly::BasicBlockLabel::new(target_true.0),
                    });
            }
        },
        // TODO: make sure that the returned binding is in the place it should.
        BlockEnd::Return(ret) => output = output.chain_one(assembly::Instruction::Ret),
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
        // codegen has nothing to do with this.
        Value::Allocate { .. } => AssemblyOutput::new(),
        // codegen won't do anything here with phi nodes. They are analyzed separately
        // and the register allocator is responsible for putting all the bindings in the same place
        Value::Phi { .. } => AssemblyOutput::new(),
        Value::Cmp {
            condition,
            lhs,
            rhs,
        } => AssemblyOutput::from(assembly::Instruction::Cmp {
            register: assembly::Register::from_id(registers[&lhs], assembly::BitSize::Bit32),
            data: could_be_constant_to_data(rhs, registers),
        })
        .chain_one(assembly::Instruction::Cset {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            condition,
        }),
        Value::Load {
            mem_binding,
            byte_size: _, // TODO: use different instruction/register size depending on byte size
        } => assembly::Instruction::Ldr {
            register: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            address: memory[&mem_binding],
        }
        .into(),
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
        Value::And { lhs, rhs } => assembly::Instruction::And {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            lhs: assembly::Register::from_id(registers[&lhs], assembly::BitSize::Bit32),
            rhs: could_be_constant_to_data(rhs, registers),
        }
        .into(),
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
