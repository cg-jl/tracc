use std::collections::HashMap;
use std::collections::HashSet;

use super::{BasicBlock, Binding, BlockBinding, BranchingMap, Statement, Value, IR};
mod binding_usage;
pub mod lifetimes;

// TODO: output some information on phi nodes per block edge between parent/child.

pub use lifetimes::{
    compute_lifetime_collisions, compute_lifetimes, CollisionMap, Lifetime, LifetimeMap,
};

pub use binding_usage::{get_usage_map, BindingUsage, UsageMap};

pub fn order_by_deps(ir: &IR, bindings: impl Iterator<Item = Binding>) -> Vec<Binding> {
    let mut all_bindings: HashMap<_, HashSet<_>> = bindings
        .map(|binding| {
            (
                binding,
                find_assignment_value(&ir.code, binding)
                    .unwrap()
                    .binding_deps()
                    .into_iter()
                    .collect(),
            )
        })
        .collect();

    let mut result = Vec::new();

    while let Some(next) = all_bindings
        .keys()
        .cloned()
        .find(|binding| all_bindings[binding].is_empty())
    {
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
        backwards.into_iter().find(|b| **b > block).is_none()
    } else {
        true
    }
}

// leaf blocks are blocks that have predecessors but aren't parents of anything
pub fn find_leaf_blocks<'code>(
    forward_map: &'code BranchingMap,
    backwards_map: &'code BranchingMap,
) -> impl Iterator<Item = BlockBinding> + 'code {
    backwards_map
        .keys()
        .copied()
        .filter(move |key| !forward_map.contains_key(key))
}

pub fn is_indirect_child_of(
    ir: &IR,
    possible_child: BlockBinding,
    possible_parent: BlockBinding,
) -> bool {
    if possible_child == possible_parent {
        false
    } else {
        antecessors(ir, possible_parent).any(|indirect_child| indirect_child == possible_child)
    }
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

pub fn antecessors(ir: &IR, binding: BlockBinding) -> impl Iterator<Item = BlockBinding> + '_ {
    BottomTopTraversal {
        backwards_map: &ir.backwards_map,
        visited: HashSet::new(),
        queue: vec![binding],
    }
}
pub struct BottomTopTraversal<'code> {
    backwards_map: &'code BranchingMap,
    visited: HashSet<BlockBinding>,
    queue: Vec<BlockBinding>,
}

impl<'code> BottomTopTraversal<'code> {
    fn new(ir: &'code IR, queue: Vec<BlockBinding>) -> Self {
        Self {
            backwards_map: &ir.backwards_map,
            visited: HashSet::new(),
            queue,
        }
    }
}

impl<'code> From<&'code IR> for BottomTopTraversal<'code> {
    fn from(ir: &'code IR) -> Self {
        Self::new(
            ir,
            find_leaf_blocks(&ir.forward_map, &ir.backwards_map).collect(),
        )
    }
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

pub fn predecessors(ir: &IR, block: BlockBinding) -> TopBottomTraversal {
    TopBottomTraversal::new(ir, vec![block])
}

pub fn predecessors_filtering_branches<'ir>(
    ir: &'ir IR,
    block_from: BlockBinding,
    mut continue_branch: impl FnMut(BlockBinding) -> bool + 'ir,
) -> impl Iterator<Item = BlockBinding> + 'ir {
    let mut queue = vec![block_from];
    let mut visited = HashSet::new();
    std::iter::from_fn(move || {
        let next = queue.pop().filter(|block| visited.insert(*block))?;
        if continue_branch(next) {
            queue.extend(ir.forward_map.get(&next).into_iter().flatten().copied());
        }
        Some(next)
    })
}

pub struct TopBottomTraversal<'code> {
    /// the code graph
    forward_map: &'code BranchingMap,
    /// visited set to avoid loops
    visited: HashSet<BlockBinding>,
    /// a queue to know what we have yet to process
    queue: Vec<BlockBinding>,
}

impl<'ir> TopBottomTraversal<'ir> {
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
            Self::new(code, vec![BlockBinding(0)])
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
