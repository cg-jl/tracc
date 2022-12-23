use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::ControlFlow;

use self::lifetimes::BlockAddress;

use super::{BasicBlock, Binding, BlockBinding, BranchingMap, Statement, Value, IR};
mod binding_usage;
pub mod lifetimes;

// TODO: output some information on phi nodes per block edge between parent/child.

pub use lifetimes::{CollisionMap, Lifetime, LifetimeMap};

pub use binding_usage::{get_usage_map, BindingUsage, UsageMap};

pub fn order_by_deps(ir: &IR, bindings: impl Iterator<Item = Binding>) -> Vec<Binding> {
    let mut all_bindings: BTreeMap<_, HashSet<_>> = bindings
        .map(|binding| {
            let assigned_value = find_assignment_value(&ir.code, binding).unwrap();
            let mut deps = HashSet::new();
            assigned_value.visit_value_bindings(&mut |dep| {
                deps.insert(dep);
                std::ops::ControlFlow::<(), _>::Continue(())
            });
            (binding, deps)
        })
        .collect();

    let mut result = Vec::new();

    while let Some(next) = all_bindings.keys().cloned().find(|binding| {
        all_bindings[binding].is_empty()
            || all_bindings[binding]
                .iter()
                .all(|other| !all_bindings.contains_key(other))
    }) {
        all_bindings.remove(&next);
        all_bindings.values_mut().for_each(|set| {
            set.remove(&next);
        });
        result.push(next);
    }

    result
}

pub fn can_block_be_removed(ir: &IR, block: BlockBinding) -> bool {
    // a block can be deleted if all the blocks that refer to it come before it
    if let Some(backwards) = ir.backwards_map.get(&block) {
        !backwards.iter().any(|b| *b > block)
    } else {
        true
    }
}

// leaf blocks are blocks that have predecessors but aren't parents of anything
pub fn find_leaf_blocks<'code>(
    forward_map: &'code BranchingMap,
    backwards_map: &'code BranchingMap,
    roots: &'code [BlockBinding],
) -> impl Iterator<Item = BlockBinding> + 'code {
    backwards_map
        .keys()
        .copied()
        .filter(move |key| !forward_map.contains_key(key))
        .chain(
            roots
                .iter()
                .copied()
                .filter(|root| !forward_map.contains_key(root)),
        )
}

pub fn is_indirect_child_of(
    ir: &IR,
    possible_child: BlockBinding,
    possible_parent: BlockBinding,
) -> bool {
    if possible_child == possible_parent {
        false
    } else {
        antecessors(ir, possible_child).any(|indirect_parent| indirect_parent == possible_parent)
    }
}

#[inline]
pub fn statements<'i>(ir: &'i IR) -> impl Iterator<Item = &'i Statement> + 'i {
    ir.code.iter().flat_map(|block| block.statements.iter())
}

pub fn statements_with_addresses<'i>(
    ir: &'i IR,
) -> impl Iterator<Item = (&'i Statement, BlockAddress)> + '_ {
    ir.code.iter().enumerate().flat_map(|(block_index, block)| {
        block
            .statements
            .iter()
            .enumerate()
            .map(move |(statement_index, statement)| {
                (
                    statement,
                    BlockAddress {
                        block: BlockBinding(block_index),
                        statement: statement_index,
                    },
                )
            })
    })
}

// TODO: register where each binding is declared when inserting blocks
pub fn find_assignment_value_with_adress(
    code: &[BasicBlock],
    binding: Binding,
) -> Option<(&Value, lifetimes::BlockAddress)> {
    code.iter()
        .enumerate()
        .flat_map(|(block_i, block)| {
            block
                .statements
                .iter()
                .enumerate()
                .map(move |(statement_i, stmt)| {
                    (
                        lifetimes::BlockAddress {
                            block: BlockBinding(block_i),
                            statement: statement_i,
                        },
                        stmt,
                    )
                })
        })
        .find_map(|(address, stmt)| {
            if let Statement::Assign { index, value } = stmt {
                if index == &binding {
                    Some((value, address))
                } else {
                    None
                }
            } else {
                None
            }
        })
}

pub fn find_assignment_value(code: &[BasicBlock], binding: Binding) -> Option<&Value> {
    code.iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|statement| {
            if let Statement::Assign { index, value } = statement {
                if index == &binding {
                    Some(value)
                } else {
                    None
                }
            } else {
                None
            }
        })
}

pub fn antecessors<'code>(ir: &'code IR, binding: BlockBinding) -> BottomTopTraversal<'code> {
    BottomTopTraversal::new(
        ir,
        ir.backwards_map
            .get(&binding)
            .into_iter()
            .flat_map(|x| x.iter().copied())
            .collect(),
    )
}

pub struct BottomTopTraversal<'code> {
    backwards_map: &'code BranchingMap,
    visited: HashSet<BlockBinding>,
    queue: Vec<BlockBinding>,
}

impl<'code> BottomTopTraversal<'code> {
    pub fn new(ir: &'code IR, queue: Vec<BlockBinding>) -> Self {
        Self {
            backwards_map: &ir.backwards_map,
            visited: HashSet::new(),
            queue,
        }
    }

    pub fn finish_and_get_visited_set(mut self) -> HashSet<BlockBinding> {
        while self.next().is_some() {}
        self.visited
    }
}

impl<'code> From<&'code IR> for BottomTopTraversal<'code> {
    fn from(ir: &'code IR) -> Self {
        Self::new(
            ir,
            find_leaf_blocks(&ir.forward_map, &ir.backwards_map, &ir.function_entrypoints)
                .collect(),
        )
    }
}

pub fn fill_indirect_parents(ir: &IR) -> HashMap<BlockBinding, HashSet<BlockBinding>> {
    let mut map = HashMap::<BlockBinding, HashSet<_>>::new();
    for root in ir.function_entrypoints.iter().copied() {
        let mut traversal = flow_order_traversal(ir, root);
        let _ = traversal.next();
        while let Some(child) = traversal.next() {
            map.entry(child)
                .or_default()
                .extend(traversal.visited.iter().copied());
        }
    }
    map
}

pub fn antecessors_filtering_branches<'ir>(
    ir: &'ir IR,
    block: BlockBinding,
    mut filter_branch: impl FnMut(BlockBinding) -> bool + 'ir,
) -> impl Iterator<Item = BlockBinding> + 'ir {
    let mut queue = vec![block];
    let mut visited = HashSet::new();
    std::iter::from_fn(move || {
        Some(loop {
            let n = queue.pop()?;
            if visited.insert(n) && filter_branch(n) {
                break n;
            }
        })
    })
}

pub fn iterate_with_bindings(
    code: &[BasicBlock],
) -> impl Iterator<Item = (BlockBinding, &BasicBlock)> {
    code.iter()
        .enumerate()
        .map(|(index, block)| (BlockBinding(index), block))
}

impl<'code> Iterator for BottomTopTraversal<'code> {
    type Item = BlockBinding;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.queue.pop()?;
        self.visited.insert(next);
        let visited_ref = &self.visited;
        let parents = self
            .backwards_map
            .get(&next)
            .into_iter()
            .flatten()
            .filter(|x| !visited_ref.contains(x))
            .copied();
        self.queue.extend(parents);
        Some(next)
    }
}

pub fn full_flow_traversal(ir: &IR) -> TopBottomTraversal {
    TopBottomTraversal::new(ir, ir.function_entrypoints.clone())
}

pub fn flow_order_traversal(ir: &IR, block: BlockBinding) -> TopBottomTraversal {
    TopBottomTraversal::new(ir, vec![block])
}

/// Same as `flow_order_traversal`, but accepts only the required parts to initialize the traversal
/// iterator. Used in parts where some part of the IR is being mutated and some part is being
/// exclusively read.
pub fn flow_order_traversal_from_parts(
    forward_map: &BranchingMap,
    block: BlockBinding,
) -> TopBottomTraversal {
    TopBottomTraversal::from_parts(forward_map, vec![block])
}

pub struct TopBottomTraversal<'code> {
    /// the code graph
    forward_map: &'code BranchingMap,
    /// visited set to avoid loops
    pub visited: HashSet<BlockBinding>,
    /// a queue to know what we have yet to process
    queue: Vec<BlockBinding>,
}

impl<'ir> TopBottomTraversal<'ir> {
    fn from_parts(forward_map: &'ir BranchingMap, queue: Vec<BlockBinding>) -> Self {
        Self {
            forward_map,
            queue,
            visited: HashSet::new(),
        }
    }

    fn new(ir: &'ir IR, queue: Vec<BlockBinding>) -> Self {
        Self {
            forward_map: &ir.forward_map,
            queue,
            visited: HashSet::new(),
        }
    }
    // exclude a block from being processed for its children (i.e add it to the visited set and remove it from the
    // queue)
    pub fn exclude(&mut self, exclude_block: BlockBinding) {
        self.queue.retain(|x| x != &exclude_block);
        self.visited.insert(exclude_block);
    }

    pub fn exclude_flatten<T, I: IntoIterator<Item = T> + 'ir>(
        mut self,
        mut mapper: impl FnMut(BlockBinding) -> I + 'ir,
    ) -> impl Iterator<Item = T> + 'ir {
        let mut current = None;
        std::iter::from_fn(move || {
            if current.is_none() {
                let next_block = self.next()?;
                current = Some(mapper(next_block).into_iter());
            }
            let curr_iter = current.as_mut()?;
            if let Some(item) = curr_iter.next() {
                Some(item)
            } else {
                current = None;
                None
            }
        })
    }

    // exclude entire branches from running (not the same as filtering the list, excluding a block
    // will exclude the entire graph)
    pub fn excluding(
        mut self,
        mut exclude_fn: impl FnMut(BlockBinding) -> bool + 'ir,
    ) -> impl Iterator<Item = BlockBinding> + 'ir {
        std::iter::from_fn(move || {
            let next = self.next()?;
            if exclude_fn(next) {
                self.exclude(next);
                None
            } else {
                Some(next)
            }
        })
    }
}

impl<'code> From<&'code IR> for TopBottomTraversal<'code> {
    fn from(code: &'code IR) -> Self {
        if code.code.is_empty() {
            Self::new(code, vec![])
        } else {
            Self::new(code, code.function_entrypoints.clone())
        }
    }
}

impl<'code> Iterator for TopBottomTraversal<'code> {
    type Item = BlockBinding;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.queue.pop()?; // no queue, no worries

        let children = self
            .forward_map
            .get(&next)
            .into_iter()
            .flatten()
            .copied()
            .filter(|x| !self.visited.contains(x));
        // extend the queue with the children as we know the parent is already yielded
        self.queue.extend(children);
        self.visited.insert(next);
        Some(next)
    }
}

// parent-child order has a queue and it won't return/compute anything once everything has been
// iterated
impl std::iter::FusedIterator for TopBottomTraversal<'_> {}
