pub mod assembly;
pub mod has_binding;
mod output;
use self::assembly::Condition;

// TODO: change output for a better builder (block based, receives IR branching maps for finishing)
use super::allocators::*;
use super::intermediate::*;
use output::AssemblyOutput;

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

pub fn codegen_function(function_name: String, mut ir: IR) -> AssemblyOutput {
    tracing::trace!(target: "codegen", "begin codegen for {function_name}()");
    tracing::debug!(target: "codegen", "{ir:?}");
    let collisions = crate::intermediate::analysis::compute_lifetime_collisions(
        &ir,
        &crate::intermediate::analysis::compute_lifetimes(&ir),
    );

    // TODO: integrate register spill output
    let registers::CodegenHints {
        need_move_to_return_reg,
        save_upon_call,
        mut completely_spilled,
        mut registers,
        stores_condition,
    } = registers::alloc_registers(
        &ir,
        analysis::order_by_deps(&ir, collisions.keys().cloned()),
        registers::make_allocator_hints(&ir),
    );

    let alloc_map = memory::make_alloc_map(&ir.code);

    alloc_map.keys().cloned().for_each(|allocated_binding| {
        registers.insert(allocated_binding, assembly::RegisterID::StackPointer);
        completely_spilled.remove(&allocated_binding);
    });

    debug_assert!(completely_spilled.is_empty(), "shouldn't have any spills");

    let (memory, mem_size) = memory::figure_out_allocations(&ir, alloc_map);

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

    debug_assert!(save_upon_call.is_empty(), "TODO: implement save upon call");

    debug_assert!(
        need_move_to_return_reg.is_empty(),
        "TODO: implement moves to return register (or generic move to register)"
    );

    // align the stack to 16 bytes
    let mem_size = 16 * ((mem_size as f64 / 16.0f64).ceil() as usize);

    // if mem size is not zero, create an epilogue  block with a label and
    // move all returns to the epilogue block.

    // collect all the blocks and their ends
    let (mut blocks, mut ends): (Vec<_>, Vec<_>) = ir
        .code
        .into_iter()
        .map(|BasicBlock { statements, end }| {
            let block = compile_block(statements, &memory, &registers, &stores_condition);
            (block, end)
        })
        .unzip();

    // change all blocks to branch to epilogue
    let prologue = if mem_size != 0 {
        ends.iter_mut().for_each(|end| {
            use assembly::Label::Epilogue;

            if let BlockEnd::Return(_) = end {
                *end = BlockEnd::Branch(Branch::Unconditional {
                    // block ID of epilogue is the new block.
                    target: BlockBinding(blocks.len()),
                });
            }
        });

        let epilogue = AssemblyOutput::from(assembly::Instruction::Add {
            target: assembly::Register::StackPointer,
            lhs: assembly::Register::StackPointer,
            rhs: assembly::Data::Immediate(mem_size as i32),
        })
        .chain_one(assembly::Instruction::Ret);

        blocks.push(epilogue);
        AssemblyOutput::from(assembly::Instruction::Sub {
            target: assembly::Register::StackPointer,
            lhs: assembly::Register::StackPointer,
            rhs: assembly::Data::Immediate(mem_size as i32),
        })
    } else {
        AssemblyOutput::new()
    };

    // TODO: for each end, reverse the condition if true_branch == current_block + 1
    // also reorder block names so that each block is nearest to the ones that branch to it.
    // all thot to avoid beq; b
    // but before that, add the epilogue block and change the return ends to branch to the
    // epilogue.

    // search backwards for empty blocks so we can delete them and make everything else branch to
    // the next block of the empty one
    // {
    //     let mut i = 0;
    //     while i != blocks.len() {
    //         let index = blocks.len() - i - 1;
    //         if blocks[index].is_empty() {
    //             // rewire all ends to one less
    //             ends.iter_mut().for_each(|end| {
    //                 for move_index in (index + 1..blocks.len()).rev() {
    //                     // UNSAFE: safe. The block is about to be deleted.
    //                     unsafe {
    //                         refactor::end_rename_block(
    //                             end,
    //                             BlockBinding(move_index + 1),
    //                             BlockBinding(move_index),
    //                         )
    //                     };
    //                 }
    //                 // if a conditional branch ends with both blocks being the same, it means it
    //                 // was just executing conditionally a block that ended up not having any source
    //                 // code. TODO: this probably happens due to phi nodes and their targets already
    //                 // being there. When phi nodes can't be allocated the same target then these
    //                 // blocks won't be empty.
    //                 if let BlockEnd::Branch(Branch::Conditional {
    //                     target_true,
    //                     target_false,
    //                     ..
    //                 }) = end
    //                 {
    //                     debug_assert_ne!(
    //                         target_true, target_false,
    //                         "conditional branch ended up folded"
    //                     );
    //                     // if target_true == target_false {
    //                     //     *end = BlockEnd::Branch(Branch::Unconditional {
    //                     //         target: *target_true,
    //                     //     })
    //                     // }
    //                 }
    //             });
    //             blocks.remove(index);
    //             ends.remove(index);
    //         } else {
    //             i += 1;
    //         }
    //     }
    // }

    let blocks_len = blocks.len();

    let get_label = |index: usize| -> assembly::Label {
        if index == blocks_len.saturating_sub(1) && mem_size != 0 {
            assembly::Label::Epilogue
        } else {
            assembly::Label::Block { num: index }
        }
    };

    let mut needed_labels = HashSet::new();
    blocks
        .iter_mut()
        .enumerate()
        .zip(ends)
        .for_each(|((index, block), end)| {
            match end {
                BlockEnd::Return(_) => {
                    block.push_back(assembly::Instruction::Ret);
                }
                BlockEnd::Branch(Branch::Unconditional { target }) => {
                    if target.0 != index + 1 {
                        needed_labels.insert(target.0);
                        block.push_back(assembly::Instruction::Branch(
                            assembly::Branch::Unconditional {
                                register: None,
                                label: get_label(target.0),
                            },
                        ));
                    }
                }
                // TODO: flag condition hints to codegen
                BlockEnd::Branch(Branch::Conditional {
                    flag,
                    target_true,
                    target_false,
                }) => {
                    let (condition, flag_is_condition) = stores_condition
                        .get(&flag)
                        .copied()
                        .map(|c| (c, true))
                        .unwrap_or((assembly::Condition::NotEquals, false));

                    let (condition, condition_target, rest_target) = if target_true.0 == index + 1 {
                        (condition.opposite(), target_false.0, target_true.0)
                    } else {
                        needed_labels.insert(target_true.0);
                        (condition, target_true.0, target_false.0)
                    };
                    needed_labels.insert(condition_target);

                    if !flag_is_condition {
                        block.push_back(assembly::Instruction::Cmp {
                            register: assembly::Register::from_id(
                                registers[&flag],
                                assembly::BitSize::Bit32,
                            ),
                            data: assembly::Data::Register(assembly::Register::ZeroRegister {
                                bit_size: assembly::BitSize::Bit32,
                            }),
                        });
                    }

                    block.push_back(assembly::Branch::Conditional {
                        condition,
                        label: get_label(condition_target),
                    });

                    if rest_target != index + 1 {
                        needed_labels.insert(rest_target);
                        block.push_back(assembly::Branch::Unconditional {
                            register: None,
                            label: get_label(rest_target),
                        });
                    }
                }
            }
        });

    for block in needed_labels {
        blocks[block].push_front(get_label(block));
    }

    blocks
        .into_iter()
        .fold(prologue, |acc, next| acc.chain(next))
        // declare function as global for linkage
        .cons(assembly::Assembly::Label(function_name.clone()))
        .cons(assembly::Directive::Type(
            function_name.clone(),
            "function".into(),
        ))
        .cons(assembly::Directive::Global(function_name))
}

fn compile_block(
    block: Vec<Statement>,
    memory: &memory::MemoryMap,
    registers: &registers::RegisterMap,
    stores_condition: &HashMap<Binding, Condition>,
) -> AssemblyOutput {
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
