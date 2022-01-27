use super::{BlockBinding, BranchingMap};
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
