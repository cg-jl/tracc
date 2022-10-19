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
    cleanup::run_safe_cleanup(&mut ir);
    while try_merge(&mut ir) {
        cleanup::prune_unreached_blocks(&mut ir);
    }
    cleanup::run_safe_cleanup(&mut ir);
    ir
}

fn repr_into_i32(repr: u32) -> i32 {
    // safely converts to u32 representation so no bits are missed and then interprets the
    // resulting 32-bits as two's complement.
    unsafe { std::mem::transmute(repr) }
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
    cleanup::run_safe_cleanup(ir);

    cleanup::prune_unreached_blocks(ir);

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
        // before renaming, we set the predecessor
        block_set_predecessor(&mut ir[child], parent);
        unsafe { refactor::rename_block(ir, child, parent) };
        let child_block = unsafe { refactor::remove_block(ir, child) };
        merge_blocks(&mut ir[parent], child_block, parent);
    }

    did_merge
}

fn block_set_predecessor(block: &mut BasicBlock, predecessor: BlockBinding) {
    block.statements = std::mem::take(&mut block.statements)
        .into_iter()
        .map(|statement| {
            if let Statement::Assign {
                index,
                value: Value::Phi { nodes },
            } = statement
            {
                let bind = nodes
                    .into_iter()
                    .find_map(|descriptor| {
                        if descriptor.block_from == predecessor {
                            Some(descriptor.value)
                        } else {
                            None
                        }
                    })
                    .expect("Inlining with no phi node data");
                Statement::Assign {
                    index,
                    value: Value::Binding(bind),
                }
            } else {
                statement
            }
        })
        .collect();
}

fn merge_blocks(
    predecessor: &mut BasicBlock,
    mut next: BasicBlock, // NOTE: the `next` block has to have a proof that it can be removed, that's why I take it by value.
    predecessor_binding: BlockBinding,
) {
    predecessor.end = next.end;
    predecessor.statements.extend(next.statements);
    cleanup::remove_aliases_in_same_block(predecessor);
    // TODO: set predecessor to the other basic block in the predecessor if there's a loop between
}

fn find_potential_folds(code: &[Statement]) -> (Vec<(usize, Binding, i32)>, HashMap<Binding, i32>) {
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
        let found_constants = &found_constants;

        statement.visit_value_bindings(&mut |binding| {
            if let Some(value) = found_constants.get(&binding).cloned() {
                folds.push((index, binding, value));
            }
            std::ops::ControlFlow::<(), ()>::Continue(())
        });
    }
    (folds, found_constants)
}

fn fold_block(ir: &mut IR, block: BlockBinding) {
    // fold as much of the statements as possible
    // let mut start_index = 0;
    let mut failed_folds: HashMap<usize, HashSet<(Binding, i32)>> = HashMap::new();
    loop {
        // collect into a vec to avoid reference issues
        let (mut potential_folds, found_constants) = find_potential_folds(&ir[block].statements);

        potential_folds.retain(|(index, binding, value)| {
            failed_folds
                .get(index)
                .filter(|set| set.contains(&(*binding, *value)))
                .is_none()
        });

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
            } = statement_propagate_constant(&found_constants, old_statement);
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

impl<T, U, E> From<Result<U, E>> for PropagationResult<T>
where
    T: From<U>,
    T: From<E>,
{
    fn from(value: Result<U, E>) -> Self {
        match value {
            Ok(v) => Self::modified(v.into()),
            Err(v) => Self::unchanged(v.into()),
        }
    }
}

impl<T> PropagationResult<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> PropagationResult<U> {
        PropagationResult {
            value: f(self.value),
            modified: self.modified,
        }
    }

    fn from_result(res: Result<T, T>) -> Self {
        match res {
            Ok(value) => Self::modified(value),
            Err(value) => Self::unchanged(value),
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
    known_constants: &HashMap<Binding, i32>,
    statement: Statement,
) -> PropagationResult<Statement> {
    match statement {
        Statement::Assign { index, value } => value_propagate_constant(known_constants, value)
            .map(|value| Statement::Assign { index, value }),
        // stores can't be folded further.
        Statement::Store { .. } => PropagationResult::unchanged(statement),
    }
}

fn could_be_constant_propagate(
    known_binding: Binding,
    known_value: i32,
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
    known_constants: &HashMap<Binding, i32>,
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
            fn eval_condition(condition: Condition, lhs: i32, rhs: i32) -> i32 {
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
                CouldBeConstant::Constant(ctant) if let Some(value) = known_constants.get(&lhs) => {
                    PropagationResult::modified(Value::Constant(eval_condition(
                        condition,
                        *value,
                        ctant,
                    )))
                }
                CouldBeConstant::Binding(other) => {

                    let known_lhs = known_constants.get(&lhs).ok_or(lhs);
                    let known_rhs = known_constants.get(&other).ok_or(other);

                    match (known_lhs, known_rhs) {
                        (Ok(lhs), Ok(rhs)) => PropagationResult::modified(Value::Constant(eval_condition(condition, *lhs, *rhs))),
                        // we have half the thing
                        (Err(lhs), Ok(rhs)) => PropagationResult::modified(Value::Cmp {
                            condition,
                            lhs,
                            rhs: (*rhs).into(),
                        }),
                        // if the condition is commutative, we can flip the arguments
                        (Ok(&rhs), Err(lhs)) if condition.is_commutative() => PropagationResult::modified(Value::Cmp {
                            condition,
                            lhs,
                            rhs: rhs.into(),
                        }),
                        _ => PropagationResult::unchanged(value),
                    }
                }
                CouldBeConstant::Constant(_) => PropagationResult::unchanged(value),
            }
        }
        Value::Load {
            mem_binding,
            byte_size,
        } => todo!(),
        Value::Negate { binding } => known_constants.get(&binding).copied().map(|x| (!x).wrapping_add(1)).ok_or(value).into(),

        Value::FlipBits { binding } => known_constants.get(&binding).copied().map(|x| !x).ok_or(value).into(),

        Value::Add { lhs, rhs } => match rhs {
            CouldBeConstant::Constant(c) => known_constants.get(&lhs).copied().map(|a| a.wrapping_add(c).into()),
            CouldBeConstant::Binding(other) => match (get_known(known_constants, lhs), get_known(known_constants, other)) {
                (Ok(a), Ok(b)) => Some(a.wrapping_add(b).into()),
                (Ok(rhs), Err(lhs)) | (Err(lhs), Ok(rhs))  => Some(Value::Add {
                    lhs,
                    rhs: rhs.into()
                }),
                _ => None
            }
        }.ok_or(value).into(),
        Value::Subtract { lhs, rhs } => match rhs {
            CouldBeConstant::Constant(c) => known_constants.get(&lhs).copied().map(|a| a.wrapping_sub(c).into()),
            CouldBeConstant::Binding(other) => match (get_known(known_constants, lhs), get_known(known_constants, other)) {
                (Ok(a), Ok(b)) => Some(a.wrapping_sub(b).into()),
                (Err(lhs), Ok(rhs)) => Some(Value::Subtract {
                    lhs,
                    rhs: rhs.into(),
                }),
                // NOTE: Since substraction is anticommutative, flipping it around would generate
                // more instructions. If the two values end up known it'll fold it anyway.
                _ => None,
            }
        }.ok_or(value).into(),
        Value::Multiply { lhs, rhs } => match rhs {
            CouldBeConstant::Constant(c) => known_constants.get(&lhs).copied().map(|a| a.wrapping_sub(c).into()),
            CouldBeConstant::Binding(rhs) => match (get_known(known_constants, lhs), get_known(known_constants, rhs)) {
                (Ok(a), Ok(b)) => Some(a.wrapping_mul(b).into()),
                (Err(lhs), Ok(rhs)) | (Ok(rhs), Err(lhs)) => Some(Value::Multiply { lhs, rhs: rhs.into() }),
                _ => None
            }
        }.ok_or(value).into(),
        // NOTE: when dividing by zero, don't fold it. The expression is UB so we'll
        // let the user shoot themselves in the foot and insert a division by zero.
        Value::Divide {
            lhs,
            rhs,
            is_signed,
        } =>
        // NOTE: Since division does *not* support any kind of *commutativity, I cannot
        // reorder it
        {
            if let Some((a, b)) = both(
                known_constants.get(&lhs).copied(),
                known_constants.get(&rhs).copied(),
            )
            .filter(|(_, b)| *b != 0)
            {
                PropagationResult::modified(Value::Constant(a / b))
            } else {
                PropagationResult::unchanged(value)
            }
        }
        Value::Lsl { lhs, rhs } => todo!(),
        Value::Lsr { lhs, rhs } => todo!(),
        Value::And { lhs, rhs } => match rhs {
            CouldBeConstant::Constant(ctant) if let Some(value) = known_constants.get(&lhs) => {
                PropagationResult::modified(Value::Constant(*value & ctant))
            }
            CouldBeConstant::Binding(other) => {

                let known_lhs = known_constants.get(&lhs).copied().ok_or(lhs);
                let known_rhs = known_constants.get(&other).copied().ok_or(other);

                match (known_lhs, known_rhs) {
                    (Ok(a), Ok(b)) => PropagationResult::modified(Value::Constant(a & b)),
                    (Ok(a), Err(b)) | (Err(b), Ok(a)) => PropagationResult::modified(Value::And {
                        lhs: b,
                        rhs: a.into()
                    }),
                    _ => PropagationResult::unchanged(value)
                }
            },
            CouldBeConstant::Constant(_) => PropagationResult::unchanged(value),
        },
        Value::Or { lhs, rhs } => todo!(),
        Value::Xor { lhs, rhs } => todo!(),
        // already a constant, cannot fold further
        Value::Constant(_) => PropagationResult::unchanged(value),
        Value::Binding(_) => todo!(),
    }
}

#[inline]
fn both<A, B>(a: Option<A>, b: Option<B>) -> Option<(A, B)> {
    a.and_then(|a| b.map(|b| (a, b)))
}

#[inline]
fn get_known(knowns: &HashMap<Binding, i32>, binding: Binding) -> Result<i32, Binding> {
    knowns.get(&binding).copied().ok_or(binding)
}
