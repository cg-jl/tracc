//! First step of code generation is to get the stack memory usage for the code

use std::collections::{HashMap, HashSet};

use crate::intermediate::{BasicBlock, Binding, BlockBinding, Statement, Value, IR};

use super::assembly;

// even if a piece of memory is not used in a block, if a leaf of it uses that piece of memory, the
// block must keep that memory alive.

type DependencyMap = HashMap<BlockBinding, Vec<Binding>>;

pub type MemoryMap = HashMap<Binding, assembly::Memory>;

fn get_dep_graph(ir: &IR) -> DependencyMap {
    ir.code
        .iter()
        .enumerate()
        .map(|(block_index, block)| {
            (
                BlockBinding(block_index),
                list_memory_deps(&block.statements),
            )
        })
        .collect()
}

// assumes to != 0
pub fn align(value: usize, to: usize) -> usize {
    if value % to == 0 {
        value
    } else {
        to * (value / to + 1)
    }
}

// what allocate_graph does:
//  - start on the leaves and follow through their preceding blocks
//  - if the place is not taken, assign that index
use crate::intermediate::analysis;

type AllocMap = HashMap<Binding, usize>;

pub fn figure_out_allocations(
    ir: &IR,
    allocations_needed: AllocMap,
    collision_map: &analysis::lifetimes::CollisionMap,
) -> (MemoryMap, usize) {
    let mut local_collisions: Vec<(_, HashSet<_>)> = collision_map
        .iter()
        .filter_map(|(k, set)| {
            if allocations_needed.contains_key(k) {
                Some((
                    *k,
                    set.iter()
                        .filter(|k| allocations_needed.contains_key(k))
                        .copied()
                        .collect(),
                ))
            } else {
                None
            }
        })
        .collect();
    local_collisions.sort_by_key(|(_, v)| v.len());
    let mut blocks: Vec<HashSet<_>> = Vec::new();

    for (binding, collisions) in local_collisions {
        // if we find some block where there are no other collisions, we can put it there.
        // otherwise we'll have to allocate a new block for it.
        if let Some(block_index) = blocks.iter().enumerate().find_map(|(i, set)| {
            // we want to know if there are no collisions in common
            if set.is_disjoint(&collisions) {
                Some(i)
            } else {
                None
            }
        }) {
            blocks[block_index].insert(binding);
        } else {
            let mut new_block = HashSet::new();
            new_block.insert(binding);
            blocks.push(new_block);
        }
    }

    // allocation is done, now we gotta transform the data into offsets

    let mut offsets = HashMap::new();
    let mut size = 0;

    for block in blocks {
        // assign the offset as the current size
        offsets.extend(block.iter().copied().map(|binding| (binding, size)));

        // now we bump the size by the max value of those allocations
        size += block
            .into_iter()
            .map(|binding| allocations_needed[&binding])
            .max()
            .unwrap();
    }

    (
        offsets
            .into_iter()
            .map(|(binding, offset)| {
                (
                    binding,
                    assembly::Memory {
                        register: assembly::Register::StackPointer,
                        offset: assembly::Offset::Determined(offset),
                    },
                )
            })
            .collect(),
        size,
    )
}

/// Make the memory allocation map for the whole code
pub fn make_alloc_map(code: &[BasicBlock]) -> AllocMap {
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

#[cfg(test)]
mod tests {}
