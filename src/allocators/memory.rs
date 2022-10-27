//! First step of code generation is to get the stack memory usage for the code

use std::collections::{HashMap, HashSet};
use tracing::{debug, span, Level};

use crate::ir::{BasicBlock, Binding, BlockBinding, Statement, Value, IR};

use crate::asmgen::assembly;
use analysis::lifetimes::BlockAddress;

// even if a piece of memory is not used in a block, if a leaf of it uses that piece of memory, the
// block must keep that memory alive.

type DependencyMap = HashMap<BlockBinding, Vec<Binding>>;

pub type MemoryMap = HashMap<Binding, assembly::Memory>;

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
use crate::ir::analysis::{self, CollisionMap, Lifetime};

type AllocMap = HashMap<Binding, usize>;

pub fn figure_out_allocations(ir: &IR, mut allocations_needed: AllocMap) -> (MemoryMap, usize) {
    let all = make_memory_lifetimes(ir, &allocations_needed);

    tracing::trace!(target: "alloc::memory", "aligning allocations: {allocations_needed:?}");
    // align all the allocations to four bytes.
    for value in allocations_needed.values_mut() {
        let rem = *value % 4;
        if rem == 0 {
            *value = *value / 4;
        } else {
            *value = *value / 4 + 1;
        }
    }

    tracing::debug!(target: "alloc::memory", "aligned allocations to blocks: {allocations_needed:?}");
    use crate::allocators::registers::ActiveBindingSet;

    let mut allocated_blocks = HashMap::new();

    // per block, let's allocate memory!
    let mut total_blocks = 0usize;
    for lifetime in all {
        let mut active = ActiveBindingSet::new();
        let mut free_blocks = HashSet::new();
        for binding in lifetime.ordered_by_start.iter().copied() {
            let start = lifetime.binding_starts[&binding];

            tracing::debug!(target: "alloc::memory::block", "allocating {binding} @{start}");

            // expire old intervals
            tracing::trace!(target: "alloc::memory::block", "currently active: {active:?}");
            let end_i = active
                .bindings
                .iter()
                .position(|other| lifetime.binding_ends[other] > start)
                .unwrap_or(active.bindings.len());

            for dropped_binding in active.bindings.drain(..end_i) {
                tracing::trace!(target: "alloc::memory::block", "freeing {dropped_binding}");
                let location = allocated_blocks[&dropped_binding];
                for i in 0..allocations_needed[&dropped_binding] {
                    free_blocks.insert(i + location);
                }
            }

            let allocated_size = allocations_needed[&binding];

            if let Some(already_allocated) = allocated_blocks.get(&binding).copied() {
                tracing::trace!(target: "alloc::memory", "{binding} already allocated to offset {already_allocated:?}");

                for block_offset in 0..allocated_size {
                    free_blocks.remove(&(block_offset + already_allocated));
                }
                active.add(binding, &lifetime.binding_ends);
                continue;
            }

            // unlike with registers, this never fails since we suppose we have unlimited memory.
            // find a spot with N consecutive free blocks.
            let found_offset = (0..=total_blocks.saturating_sub(allocated_size))
                .position(|i| (0..allocated_size).all(|offset| free_blocks.contains(&(i + offset))))
                .unwrap_or_else(|| {
                    // try to find a spot at the end where some amount of blocks is free, so we don't
                    // have to allocate extra blocks for some part of the allocation.
                    let free_block_count_at_end = (0..total_blocks)
                        .rev()
                        .take_while(|i| free_blocks.contains(i))
                        .count();
                    if free_block_count_at_end >= allocated_size {
                        tracing::warn!(target: "alloc::memory", "found needed consecutive blocks on fallback");
                        total_blocks - allocated_size
                    } else {
                        let needed_extra_alloc = allocated_size - free_block_count_at_end;
                        tracing::trace!(target: "alloc::memory", "found {free_block_count_at_end} blocks that can be reused with the {needed_extra_alloc} extra blocks");
                        let offset = total_blocks - free_block_count_at_end;
                        total_blocks += needed_extra_alloc;
                        offset
                    }
                });

            allocated_blocks.insert(binding, found_offset);

            for i in 0..allocated_size {
                // NOTE: since the blocks could be new, they won't be in the 'free' set, since
                // it makes no sense adding them to the 'free' set and removing them right after.
                free_blocks.remove(&(i + found_offset));
            }
            active.add(binding, &lifetime.binding_ends);
        }
    }

    (
        allocated_blocks
            .into_iter()
            .map(|(binding, block)| {
                (
                    binding,
                    assembly::Memory {
                        register: assembly::Register::StackPointer,
                        offset: assembly::Offset::Determined(block * 4),
                    },
                )
            })
            .collect(),
        total_blocks,
    )
}

/// Make the memory allocation map for the whole code
pub fn make_alloc_map(code: &[BasicBlock]) -> AllocMap {
    code.iter()
        .flat_map(|block| list_memory_defs(&block.statements))
        .collect()
}

// tracks per dependency where it is loaded.
pub type LoadDependencies = HashMap<Binding, HashSet<analysis::lifetimes::BlockAddress>>;

pub fn make_memory_lifetimes(
    ir: &IR,
    allocations_needed: &AllocMap,
) -> Vec<analysis::lifetimes::BlockLifetimes> {
    // memory lifetimes are more tricky because memory bindings have 'multiple' starts and ends.
    // Since the allocator accounts for bindings that it has already allocated (those just
    // contribute to the free blocks), we can push multiple starts and ends per memory. We'll also
    // have to take into account the lifetimes that pass through blocks without being used
    // explicitly inside them.
    let mut alive_memories = vec![HashSet::new(); ir.code.len()].into_boxed_slice();
    let mut all = vec![analysis::lifetimes::BlockLifetimes::default(); ir.code.len()];

    for block in analysis::TopBottomTraversal::from(ir) {
        let mut this_block_alive_mems: HashSet<_> = ir
            .backwards_map
            .get(&block)
            .into_iter()
            .flat_map(|x| x.iter().copied())
            .flat_map(|antecessor| alive_memories[antecessor.0].iter().copied())
            .collect();

        for (statement, stmt) in ir[block].statements.iter().enumerate() {
            match stmt {
                Statement::Assign {
                    value: Value::Load { mem_binding, .. },
                    ..
                }
                | Statement::Store { mem_binding, .. }
                    if allocations_needed.contains_key(mem_binding) =>
                {
                    // if we haven't encountered it already in this block, extend its lifetime
                    // to here and mark its start
                    if !all[block.0].binding_starts.contains_key(mem_binding) {
                        tracing::trace!(target: "alloc::memory::lifetimes", "found usage of {mem_binding} for the first time at {block} @{statement}");
                        for antecessor in analysis::antecessors(ir, block) {
                            alive_memories[antecessor.0].insert(*mem_binding);
                            // make the memory lifetime span to the end of each block (since it
                            // continues to live up to here)
                            all[antecessor.0]
                                .binding_ends
                                .insert(*mem_binding, ir[antecessor].statements.len());
                        }
                        all[block.0].ordered_by_start.push(*mem_binding);
                        all[block.0].binding_starts.insert(
                            *mem_binding,
                            if this_block_alive_mems.contains(mem_binding) {
                                0
                            } else {
                                statement
                            },
                        );
                    }

                    // set it as dead. Why? Well, it's the last use of the memory that we've seen.
                    all[block.0].binding_ends.insert(*mem_binding, statement);
                    this_block_alive_mems.remove(mem_binding);
                }
                _ => {}
            }
        }
        // for all the memories still alive that weren't declared here push them into it.
        for left_alive in this_block_alive_mems.iter().copied() {
            if !all[block.0].binding_starts.contains_key(&left_alive) {
                all[block.0].binding_starts.insert(left_alive, 0);
                all[block.0].ordered_by_start.push(left_alive);
                all[block.0]
                    .binding_ends
                    .insert(left_alive, ir[block].statements.len());
            }
        }

        alive_memories[block.0] = this_block_alive_mems;
    }

    for lifetimes in all.iter_mut() {
        // NOTE: using unstable sort for now, until I have no other choice than keeping traversal
        // order
        let mut taken = core::mem::take(&mut lifetimes.ordered_by_start);
        taken.sort_unstable_by_key(|k| lifetimes.binding_starts[k]);
        lifetimes.ordered_by_start = taken;
    }

    tracing::debug!(target: "alloc::memory", "gathered memory lifetimes per block: {all:#?}");

    all
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
