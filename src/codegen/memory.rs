//! First step of code generation is to get the stack memory usage for the code

use std::collections::{HashMap, HashSet};

use crate::intermediate::{BackwardsMap, BasicBlock, Binding, BlockBinding, Statement, Value, IR};

// even if a piece of memory is not used in a block, if a leaf of it uses that piece of memory, the
// block must keep that memory alive.
//
// we have a list of memory bindings with:
//  - their size
//  - their init block
//  - the blocks that use them directly

// idea:
//  - get all the leafs where each memory binding is last used
//  - propagate those uses in the following way:
//      - if a block uses some memory, and that memory is first defined in that block, it is not
//      propagated back.
//      - otherwise, all that memory that is not first defined in that block is wanted to be put at
//      the **front** of the parent block's memory (each block will expect its memory offset to
//      start at 0)
//      - in case of two or more blocks having the same parent:
//          - the common bindings are pushed to the front and reordered in the children
//          - the non-common bindings will be designated a padded zone with the maximum of the
//          added sizes and aligned to 4 bytes, and the parent's rest of bindings will have to be pushed
//          back.
//  - phases:
//      - #0. Make the initial allocations for all the blocks as if no conflicts could happen
//      (which isn't true)
//      - #1. Gather all the new stuff that is going to be added to each block in a per-branch
//      basis. (go from leaf blocks up to the top)
//      - #2. Reorder everything so that all follows the rule (I'll get into that)

pub fn debug_what_im_doing(ir: &IR) {
    dbg!(&ir.backwards_map);
    let alloc_map = make_alloc_map(&ir.code);
    let dep_graph: AllocInfo = ir
        .code
        .iter()
        .enumerate()
        .map(|(block_index, block)| {
            (
                BlockBinding(block_index),
                list_memory_deps(&block.statements)
                    .into_iter()
                    .map(AllocUnion::Single)
                    .collect(),
            )
        })
        .collect();

    dbg!(&dep_graph);
    let wanted_map = gather_branch_info(&ir.code, &ir.backwards_map, dep_graph);

    dbg!(&alloc_map);
    dbg!(&wanted_map);
}

pub type AllocInfo = HashMap<BlockBinding, Vec<AllocUnion>>;

/// an allocation union is responsible for having one or multiple bindings allocated in the same space.
/// an allocation union can have other nested structures of usings (for deeper branches)
#[derive(std::clone::Clone)]
pub enum AllocUnion {
    Single(Binding),
    Many(Vec<AllocUnion>),
}

impl AllocUnion {}

impl std::fmt::Debug for AllocUnion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AllocUnion::Single(single) => single.fmt(f),
            AllocUnion::Many(many) => {
                write!(f, "{:?}", many)
            }
        }
    }
}

// pub enum Alloc {
//     Single(Binding),
//     Branching(Vec<Binding>),
// }

fn gather_branch_info(
    code: &[BasicBlock],
    backwards_map: &BackwardsMap,
    exisiting_allocs: AllocInfo,
) -> HashMap<BlockBinding, AllocInfo> {
    let mut result: HashMap<_, AllocInfo> = HashMap::with_capacity(code.len());

    // add all the 'self' blocks
    for block_index in 0..code.len() {
        let bb = BlockBinding(block_index);
        result.insert(bb, {
            let mut info = AllocInfo::new();
            info.insert(bb, exisiting_allocs.get(&bb).cloned().unwrap_or_default());
            info
        });
    }

    for leaf_block in super::find_leaf_blocks(code) {
        let mut queue = vec![leaf_block];
        let mut visited = HashSet::new();
        while !queue.is_empty() {
            let current_leaf = queue.pop().unwrap();
            if !visited.contains(&current_leaf) {
                visited.insert(current_leaf);
                for parent in backwards_map
                    .get(&current_leaf)
                    .into_iter()
                    .flat_map(|x| x.iter())
                    .copied()
                {
                    let info = result
                        .get_mut(&parent)
                        .expect("already inserted with 'self' blocks");
                    assert!(
                        info.insert(current_leaf, exisiting_allocs[&current_leaf].clone())
                            .is_none(),
                        "should not have any value yet"
                    );
                    queue.push(parent);
                }
            }
        }
    }
    result
}

// TODO: make per-block allocation maps out of the whole allocation map and memory block's scope
// the idea is to set further-scoped memory blocks to the right and keep more local memory to the
// left

/// Allocate the stack memory that a block will need for `alloca` objects (registers don't count
/// yet)
pub fn allocate_block_memory(
    memory_usage: impl IntoIterator<Item = Binding>,
    memory_sizes: &HashMap<Binding, usize>,
) -> (usize, HashMap<Binding, usize>) {
    // the memory usage is sorted, so we just have to fill it in
    let mut map = HashMap::new();
    let mut current_offset = 0;
    for mem in memory_usage {
        map.insert(mem, current_offset);
        current_offset += memory_sizes[&mem];
    }
    (current_offset, map)
}

fn deps_per_block(code: &[BasicBlock]) -> HashMap<BlockBinding, Vec<Binding>> {
    code.iter()
        .enumerate()
        .map(|(block_index, block)| {
            (
                BlockBinding(block_index),
                list_memory_deps(&block.statements),
            )
        })
        .collect()
}

/// Make the dependency graph for memory
fn make_dependency_graph(code: &[BasicBlock]) -> HashMap<Binding, Vec<BlockBinding>> {
    let mut graph: HashMap<_, Vec<_>> = HashMap::new();

    for (block_index, block) in code.iter().enumerate() {
        let bb = BlockBinding(block_index);
        for mem in list_memory_deps(&block.statements) {
            graph.entry(mem).or_default().push(bb);
        }
    }
    graph
}

pub type AllocMap = HashMap<Binding, usize>;

/// Make the memory allocation map for the whole code
fn make_alloc_map(code: &[BasicBlock]) -> AllocMap {
    code.iter()
        .flat_map(|block| list_memory_defs(&block.statements))
        .collect()
}

/// List the memory allocations that the block defines
fn list_memory_defs(block: &[Statement]) -> impl Iterator<Item = (Binding, usize)> + '_ {
    block.iter().filter_map(|statement| {
        if let Statement::Assign {
            index,
            value: Value::Allocate { size },
        } = statement
        {
            Some((*index, *size))
        } else {
            None
        }
    })
}

/// Analyze the memory that each block of the code uses, either by store or load
fn list_memory_deps(block: &[Statement]) -> Vec<Binding> {
    let mut bindings = Vec::new();
    for statement in block {
        match statement {
            Statement::Store { mem_binding, .. } => {
                if !bindings.contains(mem_binding) {
                    bindings.push(*mem_binding)
                }
            }
            Statement::Assign {
                index: _,
                value: Value::Load { mem_binding, .. },
            } => {
                if !bindings.contains(mem_binding) {
                    bindings.push(*mem_binding)
                }
            }
            _ => (),
        }
    }
    bindings
}
