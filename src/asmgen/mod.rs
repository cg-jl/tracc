pub mod assembly;
pub mod has_binding;
mod output;
use self::assembly::Condition;

// TODO: change output for a better builder (block based, receives IR branching maps for finishing)
use super::allocators::*;
use super::ir::*;
use output::AssemblyOutput;

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Clone, Copy)]
struct BlockRange {
    pub start: BlockBinding,
    pub end: BlockBinding,
}

impl BlockRange {
    #[inline]
    pub fn contains_index(&self, index: usize) -> bool {
        index >= self.start.0 && index <= self.end.0
    }
    #[inline]
    pub fn contains(&self, binding: BlockBinding) -> bool {
        self.contains_index(binding.0)
    }

    pub fn as_range(&self) -> core::ops::RangeInclusive<usize> {
        self.start.0..=self.end.0
    }
}

impl core::fmt::Debug for BlockRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{start}..={end}", start = self.start, end = self.end)
    }
}

pub fn codegen<'code>(mut ir: IR, function_names: Vec<&'code str>) -> AssemblyOutput {
    let need_allocation = analysis::statements_with_addresses(&ir)
        .filter_map(|(s, _)| {
            if let Statement::Assign { index, .. } = s {
                Some(*index)
            } else {
                None
            }
        })
        .collect();

    let registers::CodegenHints {
        need_move_to_return_reg,
        save_upon_call,
        mut completely_spilled,
        mut registers,
        stores_condition,
    } = registers::alloc_registers(&ir, need_allocation, registers::make_allocator_hints(&ir));

    let alloc_map = memory::make_alloc_map(&ir.code);

    alloc_map.keys().cloned().for_each(|allocated_binding| {
        registers.insert(allocated_binding, assembly::RegisterID::StackPointer);
        completely_spilled.remove(&allocated_binding);
    });

    debug_assert!(completely_spilled.is_empty(), "shouldn't have any spills");

    let (memory, total_mem_size) = memory::figure_out_allocations(&ir, alloc_map.clone());

    // since the binding was moved to wzr, no need to re-move stuff here
    for binding in registers.iter().filter_map(|(binding, reg)| {
        if matches!(reg, assembly::RegisterID::ZeroRegister) {
            Some(*binding)
        } else {
            None
        }
    }) {
        unsafe {
            crate::ir::refactor::remove_binding(&mut ir, binding);
        }
    }

    debug_assert!(save_upon_call.is_empty(), "TODO: implement save upon call");
    debug_assert!(
        need_move_to_return_reg.is_empty(),
        "TODO: implement moves to return register (or generic move to register)"
    );

    #[derive(Debug)]
    struct StackInfo<'code> {
        epilogue_label: assembly::Label<'code>,
        allocation_size: usize,
    }

    let mut function_block_spans: Vec<_> = ir
        .function_entrypoints
        .iter()
        .copied()
        .map(|starting_block| {
            let ending_block = crate::ir::analysis::flow_order_traversal(&ir, starting_block)
                .filter(|block| matches!(ir.code[block.0].end, BlockEnd::Return(_)))
                .max()
                .expect("no ending block?");

            BlockRange {
                start: starting_block,
                end: ending_block,
            }
        })
        .collect();

    // register the functions that need allocation
    let mut needs_prologue_and_epilogue: HashMap<_, _> = (0..function_block_spans.len())
        .filter_map(|func_i| {
            let stack_size = {
                let statements = ir.code[function_block_spans[func_i].as_range()]
                    .iter()
                    .flat_map(|b| b.statements.iter());

                let defined_bindings = statements.filter_map(|s| {
                    if let Statement::Assign { index, .. } = s {
                        Some(index)
                    } else {
                        None
                    }
                });

                let bindings_with_offsets: Vec<_> = defined_bindings
                    .filter_map(|b| memory.get(b).map(|mem| (b, mem.offset)))
                    .collect();

                assert_eq!(
                    bindings_with_offsets.iter().map(|a| a.1).min().unwrap_or(0),
                    0,
                    "Each function's memory has to start at offset zero!"
                );

                let total_offsets = bindings_with_offsets
                    .into_iter()
                    .map(|(b, offt)| offt + alloc_map[b]);

                let raw_size = total_offsets.max().unwrap_or(0);

                let rem = raw_size % 16;
                if rem == 0 {
                    raw_size
                } else {
                    16 * (raw_size / 16 + 1)
                }
            };
            if stack_size == 0 {
                None
            } else {
                Some((
                    func_i,
                    StackInfo {
                        epilogue_label: assembly::Label::Numbered(0xDEADC0DE),
                        allocation_size: stack_size,
                    },
                ))
            }
        })
        .collect();


    let block_labels = {
        let mut label_count = 0usize;
        let mut block_labels: HashMap<_, _> = function_block_spans
            .iter()
            .map(|r| r.start)
            .zip(function_names.into_iter().map(assembly::Label::Named))
            .collect();

        let mut check_or_insert_label = |binding| {
            if !block_labels.contains_key(&binding) {
                block_labels.insert(binding, assembly::Label::Numbered(label_count));
                label_count += 1;
            }
        };

        let ends = ir.code.iter().map(|block| &block.end);
        let branches = ends.enumerate().filter_map(|(i, end)| {
            if let BlockEnd::Branch(b) = end {
                Some((i, *b))
            } else {
                None
            }
        });

        for (block_index, branch) in branches {
            match branch {
                Branch::Unconditional { target } => {
                    if target.0 != (block_index + 1) {
                        check_or_insert_label(target)
                    }
                }
                Branch::Conditional {
                    flag,
                    target_true,
                    target_false,
                } => {
                    if target_true.0 != (block_index + 1) {
                        check_or_insert_label(target_true);
                    }
                    if target_false.0 != (block_index + 1) {
                        check_or_insert_label(target_false);
                    }
                }
            }
        }

        // now get some labels for the epilogues
        for info in needs_prologue_and_epilogue.values_mut() {
            info.epilogue_label = assembly::Label::Numbered(label_count);
            label_count += 1;
        }

        block_labels
    };
    dbg!(&needs_prologue_and_epilogue);
    let mut used_epilogue_labels = HashSet::new();

    // now, let's compile everything into blocks and add epilogues/prologues wherever needed.
    let mut asm_blocks: Vec<_> = ir
        .code
        .into_iter()
        .enumerate()
        .map(|(index, block)| {
            let function_index = function_block_spans
                .iter()
                .position(|r| r.contains_index(index));
            let function_index = match function_index {
                Some(v) => v,
                None => panic!("block {index} has no assigned function?"),
            };
            let stack_info = needs_prologue_and_epilogue.get(&function_index);
            let mut compiled_block = compile_block(
                block.statements,
                &memory,
                &registers,
                &stores_condition,
                &block_labels,
            );
            match block.end {
                BlockEnd::Branch(b) => match b {
                    Branch::Unconditional { target } => {
                        if target.0 != index + 1 {
                            compiled_block.push_back(assembly::Branch::Unconditional {
                                register: None,
                                label: block_labels[&target],
                            });
                        }
                    }
                    Branch::Conditional {
                        flag,
                        target_true,
                        target_false,
                    } => {
                        let (condition, flag_is_condition) = stores_condition
                            .get(&flag)
                            .copied()
                            .map(|c| (c, true))
                            .unwrap_or((assembly::Condition::NotEquals, false));

                        tracing::trace!(target: "asmgen", "true = {target_true}, false = {target_false}, current = {index}");
                        let (condition, condition_target, rest_target) =
                            if target_true.0 == index + 1 {
                                (condition.opposite(), target_false, target_true)
                            } else {
                                (condition, target_true, target_false)
                            };


                        if !flag_is_condition {
                            compiled_block.push_back(assembly::Instruction::Cmp {
                                register: assembly::Register::from_id(
                                    registers[&flag],
                                    assembly::BitSize::Bit32,
                                ),
                                data: assembly::Data::Register(assembly::Register::ZeroRegister {
                                    bit_size: assembly::BitSize::Bit32,
                                }),
                            });
                        }

                        compiled_block.push_back(assembly::Branch::Conditional {
                            condition,
                            label: block_labels[&condition_target],
                        });


                        if rest_target.0 != index + 1 {
                            compiled_block.push_back(assembly::Branch::Unconditional {
                                register: None,
                                label: block_labels[&rest_target],
                            });
                        }
                    }
                },
                BlockEnd::Return(_) => {
                    tracing::trace!(target: "asmgen::ends", "returning from {index}, stack info: {stack_info:?}");
                    if let Some(info) = stack_info {
                         if  function_block_spans[function_index].end.0 != index 
                             /*&& !info.epilogue_label.has_number(index)*/
                        {
                            used_epilogue_labels.insert(function_index);
                            compiled_block.push_back(assembly::Branch::Unconditional {
                                register: None,
                                label: info.epilogue_label,
                            });
                        }
                    } else {
                        compiled_block.push_back(assembly::Instruction::Ret);
                    };
                }
            }

            compiled_block
        })
        .collect();

    // add block binding labels before adding the prologue/epilogue labels
    for (used_block, label) in block_labels.iter().map(|(a, b)| (*a, *b)) {
        asm_blocks[used_block.0].push_front(label);
    }

    asm_blocks.reserve_exact(needs_prologue_and_epilogue.len() * 2);

    let mut needs_prologue_and_epilogue: Vec<_> = needs_prologue_and_epilogue.into_iter().collect();
    needs_prologue_and_epilogue.sort_unstable_by_key(|k| k.0);

    tracing::trace!(target: "asmgen::epilogues", "needs_prologue_and_epilogue: {needs_prologue_and_epilogue:?}");

    let mut last_epilogue_insertion = asm_blocks.len();
    for (i, (func_i, info)) in needs_prologue_and_epilogue.into_iter().enumerate() {
        let range = &mut function_block_spans[func_i];
        dbg!(range.end);

        if last_epilogue_insertion <= range.start.0 {
            range.start.0 += 1;
            range.end.0 += 1;
        } else if last_epilogue_insertion <= range.end.0 {
            range.end.0 += 1;
        }

        asm_blocks[range.start.0].0.insert(
            1,
            assembly::Instruction::Sub {
                target: assembly::Register::StackPointer,
                lhs: assembly::Register::StackPointer,
                rhs: assembly::Data::Immediate(info.allocation_size as i32),
            }
            .into(),
        );

        // epilogue: deallocate memory and return.
        let epilogue = AssemblyOutput::from(assembly::Instruction::Add {
            target: assembly::Register::StackPointer,
            lhs: assembly::Register::StackPointer,
            rhs: assembly::Data::Immediate(info.allocation_size as i32),
        })
        .chain_one(assembly::Instruction::Ret);

        asm_blocks.insert(range.end.0 + 1, epilogue);
        last_epilogue_insertion = range.end.0 + 1;
        dbg!(range.end);

        if used_epilogue_labels.contains(&func_i)  {
            asm_blocks[range.end.0 + 1].push_front(info.epilogue_label);
        }
    }

    asm_blocks.into_iter().collect()
}

fn compile_block<'code>(
    block: Vec<Statement>,
    memory: &memory::MemoryMap,
    registers: &registers::RegisterMap,
    stores_condition: &HashMap<Binding, Condition>,
    block_labels: &HashMap<BlockBinding, assembly::Label<'code>>,
) -> AssemblyOutput<'code> {
    block
        .into_iter()
        .fold(AssemblyOutput::new(), |output, statement| {
            output.chain(match statement {
                Statement::Assign {
                    index,
                    value:
                        Value::Cmp {
                            condition,
                            lhs,
                            rhs,
                        },
                } if stores_condition.contains_key(&index) => assembly::Instruction::Cmp {
                    register: assembly::Register::from_id(
                        registers[&lhs],
                        assembly::BitSize::Bit32,
                    ),
                    data: could_be_constant_to_data(rhs, registers),
                }
                .into(),
                Statement::Assign { index, value } => {
                    let register = registers[&index];
                    compile_value(value, register, memory, registers, block_labels)
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
            })
        })
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
            assembly::Data::immediate(constant as i32, assembly::BitSize::Bit32)
        }
    }
}

fn compile_value<'code>(
    value: Value,
    target_register: assembly::RegisterID,
    memory: &memory::MemoryMap,
    registers: &registers::RegisterMap,
    block_labels: &HashMap<BlockBinding, assembly::Label<'code>>,
) -> AssemblyOutput<'code> {
    match value {
        // codegen has nothing to do with this.
        Value::Allocate { .. } => AssemblyOutput::new(),
        // codegen won't do anything here with phi nodes. They are analyzed separately
        // and the register allocator is responsible for putting all the bindings in the same place
        Value::Phi { .. } => AssemblyOutput::new(),

        Value::Call { label, args: _ } => {
            // TODO: ensure the arguments are in the correct place
            // TODO: callee-saved values!
            // TODO: move-before-call and move-after-call hints! This includes both moving arguments
            // and moving values to callee-saved registers.
            AssemblyOutput::from(assembly::Branch::Linked {
                label: block_labels[&label],
            })
        }
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
            rhs: assembly::Data::Immediate(std::i32::MAX),
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
            rhs: assembly::Register::from_id(registers[&rhs], assembly::BitSize::Bit32),
        }
        .into(),
        Value::Divide {
            lhs,
            rhs,
            is_signed,
        } => assembly::Instruction::Div {
            target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
            lhs: assembly::Register::from_id(registers[&lhs], assembly::BitSize::Bit32),
            rhs: assembly::Register::from_id(registers[&rhs], assembly::BitSize::Bit32),
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
        Value::Constant(ctant) => {
            assembly::Instruction::Mov {
                target: assembly::Register::from_id(target_register, assembly::BitSize::Bit32),
                source: assembly::Data::immediate(ctant, assembly::BitSize::Bit32),
            }
        }
        .into(),
        Value::Binding(_) => todo!(),
    }
}
