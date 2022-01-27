//! First step of code generation is to get the stack memory usage for the code

use std::collections::{HashMap, HashSet};

use crate::intermediate::{BasicBlock, Binding, BlockBinding, Statement, Value, IR};

use super::assembly;

// even if a piece of memory is not used in a block, if a leaf of it uses that piece of memory, the
// block must keep that memory alive.
//
// we have a list of memory bindings with:
//  - their size
//  - their init block
//  - the blocks that use them directly

// better idea:
//  - start on the leaves
//  - if the place is not taken, assign that index

type AllocInfo = HashMap<BlockBinding, Vec<Binding>>;

pub type MemoryMap = HashMap<Binding, assembly::Memory>;
pub type MemoryIndices = HashMap<Binding, usize>;
pub type MemoryList = Vec<assembly::Memory>;

pub fn figure_out_stack_allocs(ir: &IR) -> (MemoryMap, usize) {
    let alloc_map = make_alloc_map(&ir.code);
    let dep_graph: AllocInfo = ir
        .code
        .iter()
        .enumerate()
        .map(|(block_index, block)| {
            (
                BlockBinding(block_index),
                list_memory_deps(&block.statements),
            )
        })
        .collect();

    let (mem_description, positions) = allocate_graph(ir, dep_graph);

    // calculate the memory sizes & memory lists
    let mut memories = Vec::with_capacity(mem_description.len());

    let mut stack_offset = 0;

    for list in mem_description {
        let max_size = list
            .into_iter()
            .map(|binding| alloc_map[&binding])
            .max()
            .expect("Memory descriptions can't have empty lists");

        memories.push(assembly::Memory {
            register: assembly::Register::StackPointer,
            // determined size: the memories are listed from left to right
            offset: assembly::Offset::Determined(stack_offset),
        });
        stack_offset += max_size;
    }

    // let's merge the positions and memories into memory map that
    // can be easily accessed by the rest of codegen
    (
        positions
            .into_iter()
            .map(|(binding, memory_index)| (binding, memories[memory_index]))
            .collect(),
        stack_offset, // note: this does not align the stack bytes yet because it might need more for callee-saved registers
    )
}

// assumes to != 0
pub fn align(value: usize, to: usize) -> usize {
    if value % to == 0 {
        value
    } else {
        to * (value / to + 1)
    }
}

fn allocate_graph(
    ir: &IR,
    mut dep_graph: AllocInfo,
) -> (Vec<Vec<Binding>>, HashMap<Binding, usize>) {
    let mut commons: Vec<Vec<Binding>> = Vec::new();
    let mut alloc_graph = HashMap::new();
    use crate::intermediate::analysis;

    let mut queue: Vec<_> =
        analysis::find_leaf_blocks(&ir.forward_map, &ir.backwards_map).collect();

    // note: we need a visited set to avoid loops
    let mut visited = HashSet::new();

    while !queue.is_empty() {
        let block = queue.pop().unwrap();

        if visited.contains(&block) {
            continue;
        }

        visited.insert(block);

        // these are all the dependencies that this block has
        let deps = dep_graph.remove(&block).unwrap_or_default();

        // for each dependency:
        for dep in deps.iter().copied() {
            // if the dependency is already allocated, then we don't need to do anything
            if alloc_graph.contains_key(&dep) {
                continue;
            }

            // otherwise, we'll look at our commons map and see if we have any clashes
            // with the rest of the dependencies
            if let Some(common_index) =
                commons.iter().enumerate().find_map(|(index, common_list)| {
                    if common_list.iter().all(|mem| !deps.contains(mem)) {
                        Some(index)
                    } else {
                        None
                    }
                })
            {
                // we found an already existing list that doesn't clash!
                // set it to allocate an we're in
                alloc_graph.insert(dep, common_index);
                commons[common_index].push(dep);
            } else {
                // otherwise, we'll assign a new spot
                alloc_graph.insert(dep, commons.len());
                commons.push(vec![dep]);
            }
        }

        // add all its parents to the queue
        queue.extend(ir.backwards_map.get(&block).into_iter().flatten());
    }

    (commons, alloc_graph)
}

/// an allocation union is responsible for having one or multiple bindings allocated in the same space.
/// an allocation union can have other nested structures of usings (for deeper branches)

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

/// Make the dependency graph for memory

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
