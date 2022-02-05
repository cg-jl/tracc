use std::collections::HashSet;

use super::{BasicBlock, Binding, BlockBinding, BranchingMap, Statement, Value, IR};
mod binding_usage;

pub use binding_usage::BindingUsage;

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

pub fn predecessors(ir: &IR, binding: BlockBinding) -> impl Iterator<Item = BlockBinding> + '_ {
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

pub struct TopBottomTraversal<'code> {
    /// the code graph
    forward_map: &'code BranchingMap,
    /// visited set to avoid loops
    visited: HashSet<BlockBinding>,
    /// a queue to know what we have yet to process
    queue: Vec<BlockBinding>,
}

impl<'code> From<&'code IR> for TopBottomTraversal<'code> {
    fn from(code: &'code IR) -> Self {
        Self {
            forward_map: &code.forward_map,
            queue: vec![BlockBinding(0)],
            visited: HashSet::new(),
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
