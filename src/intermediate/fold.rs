// Constant fold IR
use super::*;
use std::collections::HashSet;

fn fold_ir_blocks(ir: &mut IR) {
    // fold each block independently
    (0..ir.code.len())
        .map(BlockBinding)
        .for_each(|binding| fold_block(ir, binding));

    // now recompute the backward/forward map
    (ir.forward_map, ir.backwards_map) = generate::generate_branching_graphs(&ir.code);
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

fn repr_into_i32(repr: u64) -> i32 {
    // safely converts to u32 representation so no bits are missed and then interprets the
    // resulting 32-bits as two's complement.
    unsafe { std::mem::transmute(repr as u32) }
}

// find places where a block jumps to another (child) block and this child only has that parent
fn find_unique_jumps(ir: &IR) -> impl Iterator<Item = (BlockBinding, BlockBinding)> + '_ {
    ir.forward_map.iter().filter_map(|(parent, children)| {
        if children.len() == 1 {
            let unique_child = children[0];
            let unique_child_parents = &ir.backwards_map[&unique_child];
            if unique_child_parents.len() == 1 {
                debug_assert_eq!(unique_child_parents[0], *parent, "Mismatch in backwards map: one block has a child who doesn't recognize it as a parent");
                Some((*parent, unique_child))
            } else { None}
        } else { None }
    })
}

fn try_merge(ir: &mut IR) -> bool {
    // make an initial folding step for all the blocks
    fold_ir_blocks(ir);

    // cleanup the code after the fold
    cleanup::run_cleanup(ir);

    // if I find a direct mapping somewhere, I inline
    let mut jumps: HashMap<_, _> = find_unique_jumps(ir).collect();

    let did_merge = !jumps.is_empty();

    fn find_noncolliding_merge(
        jumps: &mut HashMap<BlockBinding, BlockBinding>,
    ) -> Option<(BlockBinding, BlockBinding)> {
        for (parent, child) in jumps.iter().map(|(a, b)| (*a, *b)) {
            if !jumps.contains_key(&child) {
                jumps.remove(&parent);
                return Some((parent, child));
            }
        }
        None
    }

    while let Some((parent, child)) = find_noncolliding_merge(&mut jumps) {
        // rename the child block to the block it's inlined before removing it
        unsafe { refactor::rename_block(ir, child, parent) };
        let child_block = unsafe { refactor::remove_block(ir, child) };
        merge_blocks(&mut ir[parent], child_block, parent);
        cleanup::run_cleanup(ir);
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
            let bind = nodes
                .iter_mut()
                .find_map(|descriptor| {
                    if descriptor.block_from == predecessor {
                        Some(descriptor.value)
                    } else {
                        None
                    }
                })
                .expect("Inlining with no idea of phi node");
            *statement = Statement::Assign {
                index: *index,
                value: Value::Binding(bind),
            };
        }
    }
}

fn find_potential_folds(code: &[Statement]) -> impl Iterator<Item = (usize, Binding, u64)> {
    let mut found_constants = HashMap::new();

    let mut folds = Vec::new();

    for (index, statement) in code.iter().enumerate() {
        if let Statement::Assign {
            index,
            value: Value::Constant(c),
        } = statement
        {
            found_constants.insert(*index, *c);
        }
        use super::analysis::BindingUsage;
        let usages = statement.binding_deps();
        let found_constants = &found_constants;
        folds.extend(usages.into_iter().filter_map(move |binding| {
            found_constants
                .get(&binding)
                .cloned()
                .map(|value| (index, binding, value))
        }));
    }
    folds.into_iter()
}

fn fold_block(ir: &mut IR, block: BlockBinding) {
    // fold as much of the statements as possible
    // let mut start_index = 0;
    let mut failed_folds: HashMap<usize, HashSet<(Binding, u64)>> = HashMap::new();
    loop {
        // collect into a vec to avoid reference issues
        let mut potential_folds: Vec<_> = find_potential_folds(&ir[block].statements)
            .filter(|(index, binding, value)| {
                failed_folds
                    .get(index)
                    .filter(|set| set.contains(&(*binding, *value)))
                    .is_none()
            })
            .collect();

        if potential_folds.is_empty() {
            break;
        }

        let mut forget = HashSet::new();

        for (index, binding, value) in potential_folds {
            // make a replacement with a dummy
            let old_statement = std::mem::replace(
                &mut ir[block].statements[index],
                Statement::Assign {
                    index: Binding(std::usize::MAX),
                    value: Value::Allocate { size: 0 },
                },
            );

            let PropagationResult {
                value: new_statement,
                modified,
            } = statement_propagate_constant(binding, value, old_statement);
            // if the statement was modified, that means this fold succeeded, ot least partially.
            // Therefore we're going to forget all the previous bod folds from this index, to try
            // those unsucceeded folds again.
            if modified {
                forget.insert(index);
            } else {
                // mark this fold as not succeeded.
                failed_folds
                    .entry(index)
                    .or_default()
                    .insert((binding, value));
            }

            // put the new value
            std::mem::replace(&mut ir[block].statements[index], new_statement);
        }

        // now we're going to forget all of the inserted 'forget's
        forget.into_iter().for_each(|index| {
            failed_folds.remove(&index);
        });
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

struct PropagationResult<T> {
    value: T,
    modified: bool,
}

impl<T> PropagationResult<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> PropagationResult<U> {
        PropagationResult {
            value: f(self.value),
            modified: self.modified,
        }
    }

    fn modified(value: T) -> Self {
        Self {
            value,
            modified: true,
        }
    }

    fn unchanged(value: T) -> Self {
        Self {
            value,
            modified: false,
        }
    }
}

fn statement_propagate_constant(
    known_binding: Binding,
    known_value: u64,
    statement: Statement,
) -> PropagationResult<Statement> {
    match statement {
        Statement::Assign { index, value } => {
            value_propagate_constant(known_binding, known_value, value)
                .map(|value| Statement::Assign { index, value })
        }
        // stores can't be folded further.
        Statement::Store { .. } => PropagationResult::unchanged(statement),
    }
}

fn could_be_constant_propagate(
    known_binding: Binding,
    known_value: u64,
    c: CouldBeConstant,
) -> PropagationResult<CouldBeConstant> {
    match c {
        CouldBeConstant::Binding(b) if b == known_binding => {
            PropagationResult::modified(CouldBeConstant::Constant(known_value))
        }
        other => PropagationResult::unchanged(other),
    }
}

// if any of the elements have changed, the vec has changed. Otherwise it hasn't
impl<T> FromIterator<PropagationResult<T>> for PropagationResult<Vec<T>> {
    fn from_iter<U: IntoIterator<Item = PropagationResult<T>>>(iter: U) -> Self {
        iter.into_iter()
            // TODO: Propagation result proper chaining
            .fold(PropagationResult::unchanged(Vec::new()), |mut acc, next| {
                acc.value.push(next.value);
                acc.modified |= next.modified;
                acc
            })
    }
}

#[allow(unused)]
fn value_propagate_constant(
    known_binding: Binding,
    binding_value: u64,
    value: Value,
) -> PropagationResult<Value> {
    match value {
        // ollacations can't be folded further.
        Value::Allocate { .. } => PropagationResult::unchanged(value),
        // a phi node is a decision point. Until a decision is made no constant
        // is chosen.
        Value::Phi { .. } => PropagationResult::unchanged(value),
        Value::Cmp {
            condition,
            lhs,
            rhs,
        } => {
            fn eval_condition(condition: Condition, lhs: i32, rhs: i32) -> u64 {
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
                    Condition::LessThan => {
                        if lhs < rhs {
                            1
                        } else {
                            0
                        }
                    }
                    Condition::GreaterThan => {
                        if lhs > rhs {
                            1
                        } else {
                            0
                        }
                    }
                    Condition::NotEquals => {
                        if lhs != rhs {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
            match rhs {
                CouldBeConstant::Constant(ctant) if lhs == known_binding => {
                    PropagationResult::modified(Value::Constant(eval_condition(
                        condition,
                        repr_into_i32(binding_value),
                        repr_into_i32(ctant),
                    )))
                }
                CouldBeConstant::Binding(other) => {
                    if lhs == known_binding && other == known_binding {
                        let i32_value: i32 = repr_into_i32(binding_value);
                        PropagationResult::modified(Value::Constant(eval_condition(
                            condition, i32_value, i32_value,
                        )))
                    } else if other == known_binding {
                        // we know half the thing.
                        PropagationResult::modified(Value::Cmp {
                            condition,
                            lhs,
                            rhs: CouldBeConstant::Constant(binding_value),
                        })
                    } else if lhs == known_binding && condition.is_commutative() {
                        // if the condition is commutative, we can flip the thing.
                        PropagationResult::modified(Value::Cmp {
                            condition,
                            lhs: other,
                            rhs: CouldBeConstant::Constant(binding_value),
                        })
                    } else {
                        PropagationResult::unchanged(value)
                    }
                }
                CouldBeConstant::Constant(_) => PropagationResult::unchanged(value),
            }
        }
        Value::Load {
            mem_binding,
            byte_size,
        } => todo!(),
        Value::Negate { binding } => {
            if binding == known_binding {
                PropagationResult::modified(Value::Constant((!binding_value).wrapping_add(1)))
            } else {
                PropagationResult::unchanged(value)
            }
        }
        Value::FlipBits { binding } => {
            if binding == known_binding {
                PropagationResult::modified(Value::Constant(!binding_value))
            } else {
                PropagationResult::unchanged(value)
            }
        }
        Value::Add { lhs, rhs } => match (lhs, rhs) {
            (a, CouldBeConstant::Constant(c)) if a == known_binding => {
                PropagationResult::modified(Value::Constant(binding_value.wrapping_add(c)))
            }
            (a, CouldBeConstant::Binding(b)) => {
                if a == b && a == known_binding {
                    PropagationResult::modified(Value::Constant(
                        binding_value.wrapping_add(binding_value),
                    ))
                } else if a == known_binding {
                    // flip the operation to have the constant on rhs
                    PropagationResult::modified(Value::Add {
                        lhs: b,
                        rhs: CouldBeConstant::Constant(binding_value),
                    })
                } else if b == known_binding {
                    PropagationResult::modified(Value::Add {
                        lhs: a,
                        rhs: CouldBeConstant::Constant(binding_value),
                    })
                } else {
                    PropagationResult::unchanged(value)
                }
            }

            (lhs, rhs) => PropagationResult::unchanged(value),
        },
        Value::Subtract { lhs, rhs } => match rhs {
            CouldBeConstant::Constant(c) if lhs == known_binding => {
                PropagationResult::modified(Value::Constant(binding_value.wrapping_sub(c)))
            }
            CouldBeConstant::Binding(other) => {
                if lhs == known_binding && other == known_binding {
                    PropagationResult::modified(Value::Constant(0))
                } else if lhs == known_binding {
                    // since subtraction is anticommutative, I'd have to generate more values to
                    // accomodate a reorder. I'll just fail so that the order of evaluation for
                    // folds changes
                    PropagationResult::unchanged(value)
                } else if other == known_binding {
                    // here I can change the rhs to be a constant
                    PropagationResult::modified(Value::Subtract {
                        lhs,
                        rhs: CouldBeConstant::Constant(binding_value),
                    })
                } else {
                    PropagationResult::unchanged(value)
                }
            }
            CouldBeConstant::Constant(_) => PropagationResult::unchanged(value),
        },
        Value::Multiply { lhs, rhs } => match (lhs, rhs) {
            (a, CouldBeConstant::Constant(c)) if a == known_binding => {
                PropagationResult::modified(Value::Constant(binding_value.wrapping_mul(c)))
            }
            (a, CouldBeConstant::Binding(b)) => {
                if a == b && a == known_binding {
                    PropagationResult::modified(Value::Constant(
                        binding_value.wrapping_mul(binding_value),
                    ))
                } else if a == known_binding {
                    // flip the operation to have the constant on rhs
                    PropagationResult::modified(Value::Multiply {
                        lhs: b,
                        rhs: CouldBeConstant::Constant(binding_value),
                    })
                } else if b == known_binding {
                    PropagationResult::modified(Value::Multiply {
                        lhs: a,
                        rhs: CouldBeConstant::Constant(binding_value),
                    })
                } else {
                    PropagationResult::unchanged(value)
                }
            }

            (lhs, rhs) => PropagationResult::unchanged(value),
        },
        // NOTE: when dividing by zero, don't fold it. The expression is UB so we'll
        // let the user shoot themselves in the foot and insert a division by zero.
        Value::Divide {
            lhs,
            rhs,
            is_signed,
        } => match rhs {
            CouldBeConstant::Binding(other) => {
                // NOTE: Since division does *not* support any kind of *commutativity, I cannot
                // reorder it
                if lhs == known_binding && other == known_binding && binding_value != 0 {
                    PropagationResult::modified(Value::Constant(1))
                } else if other == known_binding {
                    // I can set the other to be a constant
                    PropagationResult::modified(Value::Divide {
                        lhs,
                        rhs: CouldBeConstant::Constant(binding_value),
                        is_signed,
                    })
                } else {
                    PropagationResult::unchanged(value)
                }
            }
            CouldBeConstant::Constant(ctant) if lhs == known_binding && ctant != 0 => {
                PropagationResult::modified(Value::Constant(binding_value / ctant))
            }
            // otherwise i'll leave it as is, because I can't fold it in a safe way.
            _ => PropagationResult::unchanged(value),
        },
        Value::Lsl { lhs, rhs } => todo!(),
        Value::Lsr { lhs, rhs } => todo!(),
        Value::And { lhs, rhs } => match rhs {
            CouldBeConstant::Constant(ctant) if lhs == known_binding => {
                PropagationResult::modified(Value::Constant(binding_value & ctant))
            }
            CouldBeConstant::Binding(other) => {
                if lhs == known_binding && other == known_binding {
                    PropagationResult::modified(Value::Constant(binding_value))
                } else if lhs == known_binding {
                    // reorder the AND
                    PropagationResult::modified(Value::And {
                        lhs: other,
                        rhs: binding_value.into(),
                    })
                } else if other == known_binding {
                    // we could evaluate half of it.
                    PropagationResult::modified(Value::And {
                        lhs,
                        rhs: binding_value.into(),
                    })
                } else {
                    PropagationResult::unchanged(value)
                }
            }
            CouldBeConstant::Constant(_) => PropagationResult::unchanged(value),
        },
        Value::Or { lhs, rhs } => todo!(),
        Value::Xor { lhs, rhs } => todo!(),
        // already a constant, cannot fold further
        Value::Constant(_) => PropagationResult::unchanged(value),
        Value::Binding(_) => todo!(),
    }
}
