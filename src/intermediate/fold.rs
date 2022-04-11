// Constant fold IR
use super::*;

fn fold_ir_blocks(ir: &mut IR) {
    // fold each block independently
    (0..ir.code.len())
        .map(BlockBinding)
        .for_each(|binding| fold_block(ir, binding));

    // now recompute the backward/forward map
    (ir.backwards_map, ir.forward_map) = generate::generate_branching_graphs(&ir.code);
}

pub fn constant_fold(mut ir: IR) -> IR {
    cleanup::run_cleanup(&mut ir);
    while try_merge(&mut ir) {
        // run cleanup to remove unused blocks/repeated aliases through the blocks
        cleanup::run_cleanup(&mut ir);
    }
    cleanup::run_cleanup(&mut ir);
    ir
}

fn try_merge(ir: &mut IR) -> bool {
    // make an initial folding step for all the blocks
    fold_ir_blocks(ir);

    let mut did_merge = false;

    // if I find a direct mapping somewhere, I inline
    for (predecessor, next) in ir
        .forward_map
        .iter()
        .filter_map(|(binding, others)| {
            if others.len() == 1 {
                others.into_iter().next().map(|x| (*binding, *x))
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
    {
        if analysis::can_block_be_removed(ir, next) {
            // UNSAFE: safe. The block can be safely removed as a separate block and integrated
            // into the parent.
            let next = unsafe { refactor::remove_block(ir, next) };
            merge_blocks(&mut ir[predecessor], next, predecessor);
            did_merge = true;
        }
    }

    did_merge
}

fn merge_blocks(
    predecessor: &mut BasicBlock,
    mut next: BasicBlock, // NOTE: the `next` block has to have a proof that it can be removed, that's why I take it by value.
    predecessor_binding: BlockBinding,
) {
    predecessor.end = next.end;
    block_set_only_predecessor(&mut next, predecessor_binding);
    predecessor.statements.extend(next.statements);
    // TODO: set predecessor to the other basic block in the predecessor if there's a loop between
    // them.
}

fn block_set_only_predecessor(block: &mut BasicBlock, predecessor: BlockBinding) {
    for statement in &mut block.statements {
        // set phi nodes to that predecessor
        if let Statement::Assign {
            index,
            value: Value::Phi { nodes },
        } = statement
        {
            // if we can find the binding that
            let value = nodes
                .into_iter()
                .find_map(|descriptor| {
                    if descriptor.block_from == predecessor {
                        Some(descriptor.value)
                    } else { None }
                })
                .expect("Setting only predecessor but one binding depends on a phi node that doesn't include said predecessor");

            *statement = Statement::Assign {
                index: *index,
                value: match value {
                    CouldBeConstant::Binding(binding) => Value::Binding(binding),
                    CouldBeConstant::Constant(ctant) => Value::Constant(ctant),
                },
            };
        }
    }
}

fn find_constant_in_block(block: &[Statement]) -> Option<(Binding, u64)> {
    block.into_iter().find_map(|statement| {
        if let Statement::Assign {
            index,
            value: Value::Constant(v),
        } = statement
        {
            Some((*index, *v))
        } else {
            None
        }
    })
}

fn fold_block(ir: &mut IR, block: BlockBinding) {
    // fold as much of the statements as possible
    let mut start_index = 0;
    while let Some((binding, binding_value)) =
        find_constant_in_block(&ir[block].statements[start_index..])
    {
        block_propagate_constant(binding, binding_value, &mut ir[block].statements);
        start_index += 1;
    }

    // if we got a conditional branch and the flag is known,
    // we can switch it to an unconditional branch
    if let BlockEnd::Branch(Branch::Conditional {
        flag,
        target_true,
        target_false,
    }) = ir[block].end
    {
        if let Value::Constant(c) = analysis::find_assignment_value(&ir.code, flag).unwrap() {
            ir[block].end = BlockEnd::Branch(Branch::Unconditional {
                target: if *c == 0 { target_false } else { target_true },
            })
        }
    }
}

fn block_propagate_constant(binding: Binding, binding_value: u64, block: &mut [Statement]) {
    for statement in block {
        match statement {
            Statement::Assign { index, value } => {
                // replace with a dummy allocate
                let old = std::mem::replace(value, Value::Allocate { size: 0 });
                *value = value_propagate_constant(binding, binding_value, old);
            }
            #[allow(unused)]
            Statement::Store {
                mem_binding,
                binding,
                byte_size,
            } => todo!(),
        }
    }
}

#[allow(unused)]
fn value_propagate_constant(known_binding: Binding, binding_value: u64, value: Value) -> Value {
    match value {
        Value::Allocate { .. } => value,
        Value::Phi { nodes } => Value::Phi {
            nodes: nodes
                .into_iter()
                .map(|PhiDescriptor { value, block_from }| PhiDescriptor {
                    value: match value {
                        CouldBeConstant::Binding(bind) if bind == known_binding => {
                            CouldBeConstant::Constant(binding_value)
                        }
                        other => other,
                    },
                    block_from,
                })
                .collect(),
        },
        Value::Cmp {
            condition,
            lhs,
            rhs,
        } => {
            fn eval_condition(condition: Condition, lhs: u64, rhs: u64) -> u64 {
                match condition {
                    Condition::Equals => {
                        if lhs == rhs {
                            1
                        } else {
                            0
                        }
                    }
                    Condition::LessEqual => {
                        if lhs <= rhs {
                            1
                        } else {
                            0
                        }
                    }
                    Condition::GreaterEqual => {
                        if lhs >= rhs {
                            1
                        } else {
                            0
                        }
                    }
                    other => eval_condition(other.antidote(), rhs, lhs),
                }
            }
            match (lhs, rhs) {
                (a, CouldBeConstant::Binding(b)) => {
                    if a == b && a == known_binding {
                        Value::Constant(eval_condition(condition, binding_value, binding_value))
                    } else if a == known_binding {
                        Value::Cmp {
                            // flip the condition and the arguments
                            condition: condition.antidote(),
                            lhs: b,
                            rhs: CouldBeConstant::Constant(binding_value),
                        }
                    } else if b == known_binding {
                        Value::Cmp {
                            condition,
                            lhs: a,
                            rhs: CouldBeConstant::Constant(binding_value),
                        }
                    } else {
                        value
                    }
                }
                (a, CouldBeConstant::Constant(other)) if a == known_binding => {
                    Value::Constant(eval_condition(condition, binding_value, other))
                }
                (lhs, rhs) => value,
            }
        }
        Value::Load {
            mem_binding,
            byte_size,
        } => todo!(),
        Value::Negate { binding } => {
            if binding == known_binding {
                Value::Constant((!binding_value).wrapping_add(1))
            } else {
                value
            }
        }
        Value::FlipBits { binding } => {
            if binding == known_binding {
                Value::Constant(!binding_value)
            } else {
                value
            }
        }
        Value::Add { lhs, rhs } => match (lhs, rhs) {
            (a, CouldBeConstant::Constant(c)) if a == known_binding => {
                Value::Constant(binding_value.wrapping_add(c))
            }
            (a, CouldBeConstant::Binding(b)) => {
                if a == b && a == known_binding {
                    Value::Constant(binding_value.wrapping_add(binding_value))
                } else if a == known_binding {
                    // flip the operation to have the constant on rhs
                    Value::Add {
                        lhs: b,
                        rhs: CouldBeConstant::Constant(binding_value),
                    }
                } else if b == known_binding {
                    Value::Add {
                        lhs: a,
                        rhs: CouldBeConstant::Constant(binding_value),
                    }
                } else {
                    value
                }
            }

            (lhs, rhs) => value,
        },
        Value::Subtract { lhs, rhs } => todo!(),
        Value::Multiply { lhs, rhs } => match (lhs, rhs) {
            (a, CouldBeConstant::Constant(c)) if a == known_binding => {
                Value::Constant(binding_value.wrapping_mul(c))
            }
            (a, CouldBeConstant::Binding(b)) => {
                if a == b && a == known_binding {
                    Value::Constant(binding_value.wrapping_mul(binding_value))
                } else if a == known_binding {
                    // flip the operation to have the constant on rhs
                    Value::Multiply {
                        lhs: b,
                        rhs: CouldBeConstant::Constant(binding_value),
                    }
                } else if b == known_binding {
                    Value::Multiply {
                        lhs: a,
                        rhs: CouldBeConstant::Constant(binding_value),
                    }
                } else {
                    value
                }
            }

            (lhs, rhs) => value,
        },
        // NOTE: when dividing by zero, don't fold it. The expression is UB so we'll
        // let the user shoot themselves in the foot and insert a division by zero.
        Value::Divide {
            lhs,
            rhs,
            is_signed,
        } => todo!(),
        Value::Lsl { lhs, rhs } => todo!(),
        Value::Lsr { lhs, rhs } => todo!(),
        Value::And { lhs, rhs } => todo!(),
        Value::Or { lhs, rhs } => todo!(),
        Value::Xor { lhs, rhs } => todo!(),
        // already a constant
        Value::Constant(_) => value,
        Value::Binding(_) => todo!(),
    }
}
