pub mod assembly;
pub mod has_binding;
mod output;

use self::assembly::Condition;
use crate::allocators::memory::MemBinding;
use crate::asmgen::assembly::Assembly;
use crate::asmgen::assembly::RegisterID;

// TODO: change output for a better builder (block based, receives IR branching maps for finishing)
use super::allocators::*;
use super::ir::*;
use output::AssemblyOutput;

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Clone, Copy)]
pub struct BlockRange {
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
    // now, let's get the function block ends since we're not going to modify branches anymore.
    ir.function_endpoints.extend(
        ir.function_entrypoints
            .iter()
            .copied()
            .enumerate()
            .flat_map(|(function_index, entrypoint)| {
                analysis::flow_order_traversal_from_parts(&ir.forward_map, entrypoint)
                    .filter(|block| !ir.forward_map.contains_key(block))
                    .map(move |block| (block, function_index))
            }),
    );

    let global_decls: Vec<_> = function_names
        .iter()
        .copied()
        .map(assembly::Directive::Global)
        .collect();

    let need_allocation = analysis::statements_with_addresses(&ir)
        .filter_map(|(s, _)| {
            if let Statement::Assign { index, value } = s {
                if matches!(value, Value::Allocate { .. }) {
                    None
                } else {
                    Some(*index)
                }
            } else {
                None
            }
        })
        .collect();

    let registers::CodegenHints {
        mut need_move_from_r0,
        callee_saved_per_function,
        need_move_to_return_reg,
        need_move_to_args,
        save_upon_call,
        mut completely_spilled,
        mut registers,
        stores_condition,
    } = registers::alloc_registers(&ir, need_allocation, registers::make_allocator_hints(&ir));

    assert!(
        need_move_to_args.is_empty(),
        "TODO: implement moving to the correct argument registers"
    );

    let callee_saved_per_function = {
        let mut cspf = callee_saved_per_function;

        // know if any function does a call
        for (callee_saved, function_entrypoint) in
            cspf.iter_mut().zip(ir.function_entrypoints.iter().copied())
        {
            let fn_traversal = analysis::flow_order_traversal(&ir, function_entrypoint);
            let statements = fn_traversal.flat_map(|block| &ir[block].statements);
            let values = statements.filter_map(|st| {
                if let Statement::Assign { value, .. } = st {
                    Some(value)
                } else {
                    None
                }
            });
            let has_call = values.into_iter().any(|v| matches!(v, Value::Call { .. }));
            tracing::trace!(target: "codegen::callee_saved", "function_entrypoint: {function_entrypoint} has call: {has_call}");
            if has_call {
                callee_saved.insert(0, assembly::RegisterID::from(29));
                callee_saved.insert(1, assembly::RegisterID::from(30));
            }
        }

        cspf
    };

    let alloc_map = memory::make_alloc_map(&ir.code);

    alloc_map.keys().cloned().for_each(|allocated_binding| {
        registers.insert(allocated_binding, assembly::RegisterID::StackPointer);
        completely_spilled.remove(&allocated_binding);
    });

    debug_assert!(completely_spilled.is_empty(), "shouldn't have any spills");

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
    let (memory, total_mem_size) = memory::figure_out_allocations(
        &ir,
        alloc_map.clone(),
        |func_i| callee_saved_per_function[func_i].len(),
        &function_block_spans,
    );

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
                    .filter_map(|b| memory.get(&MemBinding::IR(*b)).map(|mem| (b, mem.offset)))
                    .collect();

                let total_offsets = bindings_with_offsets
                    .into_iter()
                    .map(|(b, offt)| offt + alloc_map[b])
                    .chain(Some(callee_saved_per_function[func_i].len() * 8));

                let raw_size = total_offsets.max().unwrap();

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
                &mut need_move_from_r0,
                &stores_condition,
                &block_labels,
            );

            let is_start_of_function = function_block_spans[function_index].start.0 == index;


            if is_start_of_function {
                for (index, binding) in ir.function_argument_bindings[function_index].clone().into_iter().map(Binding).enumerate() {
                    compiled_block.push_front(assembly::Instruction::Str { register: assembly::Register::GeneralPurpose { index: index as u8, bit_size: assembly::BitSize::Bit32 } , address: memory[&MemBinding::IR(binding)] });
                }
            }


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

        if last_epilogue_insertion <= range.start.0 {
            range.start.0 += 1;
            range.end.0 += 1;
        } else if last_epilogue_insertion <= range.end.0 {
            range.end.0 += 1;
        }

        let (save_registers, load_registers) = {
            let mut load = AssemblyOutput::new();
            let mut save = AssemblyOutput::from(assembly::Instruction::Sub {
                target: assembly::Register::StackPointer,
                lhs: assembly::Register::StackPointer,
                rhs: assembly::Data::Immediate(info.allocation_size as i32),
            });
            let regids = &callee_saved_per_function[func_i];
            if let Some(start) = memory.get(&MemBinding::CalleeSaves(func_i)) {
                let mut i = 0;
                while (i + 1) < regids.len() {
                    save.push_back(assembly::Instruction::Stp {
                        a: assembly::Register::from_id(regids[i], assembly::BitSize::Bit64),
                        b: assembly::Register::from_id(regids[i + 1], assembly::BitSize::Bit64),
                        address: start.with_offset(i * 8),
                    });
                    // make sure to save the stack pointer to x29
                    // TODO: we can detect that we need to store the stack pointer before allocating!
                    // why not pass that as an allocator hint? maybe extend the allocator to not be
                    // focused on bindings but rather just on lifetimes?
                    if (regids[i] == RegisterID::GeneralPurpose { index: 29 }) {
                        save.push_back(assembly::Instruction::Mov {
                            target: assembly::Register::GeneralPurpose {
                                index: 29,
                                bit_size: assembly::BitSize::Bit64,
                            },
                            source: assembly::Data::Register(assembly::Register::StackPointer),
                        });
                    }
                    load.push_front(assembly::Instruction::Ldp {
                        a: assembly::Register::from_id(regids[i], assembly::BitSize::Bit64),
                        b: assembly::Register::from_id(regids[i + 1], assembly::BitSize::Bit64),
                        address: start.with_offset(i * 8),
                    });

                    i += 2;
                }

                if i < regids.len() {
                    save.push_back(assembly::Instruction::Str {
                        register: assembly::Register::from_id(regids[i], assembly::BitSize::Bit64),
                        address: start.with_offset(i * 8),
                    });
                    load.push_front(assembly::Instruction::Ldr {
                        register: assembly::Register::from_id(regids[i], assembly::BitSize::Bit64),
                        address: start.with_offset(i * 8),
                    });
                }
            }

            (save, load)
        };

        asm_blocks[range.start.0].insert_asm(1, save_registers);

        // epilogue: deallocate memory and return.

        let epilogue = AssemblyOutput::from(assembly::Instruction::Add {
            target: assembly::Register::StackPointer,
            lhs: assembly::Register::StackPointer,
            rhs: assembly::Data::Immediate(info.allocation_size as i32),
        })
        .chain_one(assembly::Instruction::Ret);

        let epilogue = load_registers.chain(epilogue);

        asm_blocks.insert(range.end.0 + 1, epilogue);
        last_epilogue_insertion = range.end.0 + 1;

        if used_epilogue_labels.contains(&func_i) {
            asm_blocks[range.end.0 + 1].push_front(info.epilogue_label);
        }
    }

    global_decls
        .into_iter()
        .map(AssemblyOutput::from)
        .chain(asm_blocks)
        .collect()
}

fn compile_block<'code>(
    block: Vec<Statement>,
    memory: &memory::MemoryMap,
    registers: &registers::RegisterMap,
    need_move_from_r0: &mut HashSet<Binding>,
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
                    let gen = compile_value(value, register, memory, registers, block_labels);
                    if need_move_from_r0.remove(&index) {
                        gen.chain_one(assembly::Instruction::Mov {
                            target: assembly::Register::from_id(register, assembly::BitSize::Bit32),
                            source: assembly::Data::Register(assembly::Register::GeneralPurpose {
                                index: 0,
                                bit_size: assembly::BitSize::Bit32,
                            }),
                        })
                    } else {
                        gen
                    }
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
                        address: memory[&MemBinding::IR(mem_binding)],
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

        Value::Call { label, args } => {
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
            address: memory[&MemBinding::IR(mem_binding)],
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
