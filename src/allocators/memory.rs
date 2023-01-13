//! First step of code generation is to get the stack memory usage for the code

use std::collections::{HashMap, HashSet};
use std::fmt;
use tracing::{debug, span, Level};

use crate::ir::analysis::lifetimes::BlockLifetimes;
use crate::ir::{BasicBlock, Binding, BlockBinding, BlockRange, Statement, Value, IR};

use crate::asmgen::assembly;
use analysis::lifetimes::BlockAddress;

// even if a piece of memory is not used in a block, if a leaf of it uses that piece of memory, the
// block must keep that memory alive.

type DependencyMap = HashMap<BlockBinding, Vec<Binding>>;

pub type MemoryMap = HashMap<MemBinding, assembly::Memory>;

// assumes to != 0
pub fn align(value: usize, to: usize) -> usize {
    if value % to == 0 {
        value
    } else {
        to * (value / to + 1)
    }
}

impl MemBinding {
    pub fn get_alloc_block_size<F: FnOnce(usize) -> usize>(
        &self,
        alloc_map: &AllocMap,
        used_callee_saved_registers: F,
    ) -> usize {
        match self {
            MemBinding::CalleeSaves(f) => used_callee_saved_registers(*f) * 2,
            MemBinding::IR(b) => alloc_map[b],
        }
    }
}

// what allocate_graph does:
//  - start on the leaves and follow through their preceding blocks
//  - if the place is not taken, assign that index
use crate::ir::analysis::{self, BindingUsage, CollisionMap, Lifetime};

type AllocMap = HashMap<Binding, usize>;

pub fn figure_out_allocations(
    ir: &IR,
    mut allocations_needed: AllocMap,
    used_callee_saved_registers: impl Fn(usize) -> usize,
) -> (MemoryMap, usize) {
    let mut all =
        make_memory_lifetimes(ir, &allocations_needed, &ir.function_block_ranges).into_vec();

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

    let choose_lifetime = move || {
        let best_index = (0..all.len()).max_by_key(|i| all[*i].ordered_by_start.len())?;
        Some(all.remove(best_index))
    };

    // per block, let's allocate memory!
    let mut total_blocks = 0usize;
    for lifetime in std::iter::from_fn(choose_lifetime) {
        let block_index = lifetime.block_index;
        let mut active = ActiveBindingSet::new();
        let mut free_blocks: HashSet<_> = (0..total_blocks).collect();

        // then, allocate the rest of needed bindings using linear allocation

        for binding in lifetime.ordered_by_start.iter().copied() {
            let start = lifetime.binding_starts[&binding];

            tracing::debug!(target: "alloc::memory::block", "allocating {binding} @{start}");

            // expire old intervals
            tracing::trace!(target: "alloc::memory::block", "currently active: {active:?}");
            tracing::trace!(target: "alloc::memory::block", "free blocks: {free_blocks:?}");
            let end_i = active
                .bindings
                .iter()
                .position(|other| lifetime.binding_ends[other] >= start)
                .unwrap_or(active.bindings.len());

            for dropped_binding in active.bindings.drain(..end_i) {
                tracing::trace!(target: "alloc::memory::block", "freeing {dropped_binding}");
                let location = allocated_blocks[&dropped_binding];
                for i in 0..binding
                    .get_alloc_block_size(&allocations_needed, &used_callee_saved_registers)
                {
                    free_blocks.insert(i + location);
                }
            }

            let allocated_size =
                binding.get_alloc_block_size(&allocations_needed, &used_callee_saved_registers);

            // skip empty allocations from unneeded callee saves.
            if allocated_size == 0 {
                continue;
            }

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
                .inspect(|found| tracing::trace!("found offset: {found}"))
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
                        offset: block * 4,
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
    function_block_ranges: &[BlockRange],
) -> Box<[analysis::lifetimes::BlockLifetimes<MemBinding>]> {
    let mut all = {
        let mut uall = Box::new_uninit_slice(ir.code.len());
        for (block_index, wlt) in uall.iter_mut().enumerate() {
            wlt.write(BlockLifetimes {
                block_index,
                ordered_by_start: Vec::new(),
                binding_starts: HashMap::new(),
                binding_ends: HashMap::new(),
            });
        }

        unsafe { uall.assume_init() }
    };

    for (block, maybe_parent) in analysis::track_last_parent(ir) {
        // 1. Grab the lasting values from the parent.
        if let Some(parent) = maybe_parent {
            // Assume that they are used throughout all the block for now.

            // move it directly.
            all[block.0].ordered_by_start = all[parent.0].ordered_by_start.clone();
            all[block.0].binding_starts = all[block.0]
                .ordered_by_start
                .iter()
                .copied()
                .map(|a| (a, 0))
                .collect();
        }

        // 2. Register starts and ends of values.
        for (st_index, statement) in ir[block].statements.iter().enumerate() {
            use analysis::BindingUsage;
            if let Statement::Assign {
                index,
                value: Value::Allocate { .. },
            } = statement
            {
                let mem_binding = MemBinding::IR(*index);
                all[block.0].binding_starts.insert(mem_binding, st_index);
                all[block.0].ordered_by_start.push(mem_binding);
            }

            if let Statement::Store { mem_binding, .. }
            | Statement::Assign {
                value: Value::Load { mem_binding, .. },
                ..
            } = statement
            {
                let mem_binding = MemBinding::IR(*mem_binding);
                all[block.0].binding_ends.insert(mem_binding, st_index);
            }
        }

        // 3. Register "out-of-block" ends for anything that hasn't found an end to itself here.
        for binding in all[block.0].ordered_by_start.iter().copied() {
            let _ = all[block.0]
                .binding_ends
                .try_insert(binding, ir[block].statements.len());
        }
    }

    let mut can_drop = vec![Vec::new(); ir.code.len()];
    // go through the blocks in reverse and detect where can we drop memory blocks:
    // - The memory block must not be used through the block (end == block len && start == 0)
    // - The memory block must not be used through the block's children (only "forward" children? some
    // sort of "scope" ranking might be needed for that.)
    for block in analysis::BottomTopTraversal::from(ir) {
        can_drop[block.0] = all[block.0]
            .ordered_by_start
            .iter()
            .copied()
            .filter(|b| {
                // memory block must not be used
                all[block.0].binding_ends[b] == ir[block].statements.len()
                    && all[block.0].binding_starts[b] == 0
                    && ir
                        .forward_map
                        .get(&block)
                        .into_iter()
                        .flatten()
                        .all(|child| can_drop[child.0].contains(b))
            })
            .collect();
    }

    // Now, execute the drops
    for (block_index, can_drop) in can_drop.into_iter().enumerate() {
        for dropped_binding in can_drop {
            let index = all[block_index]
                .ordered_by_start
                .iter()
                .position(|b| b == &dropped_binding);
            let index = unsafe { index.unwrap_unchecked() };
            all[block_index].ordered_by_start.remove(index);
            all[block_index].binding_starts.remove(&dropped_binding);
            all[block_index].binding_ends.remove(&dropped_binding);
        }
    }

    // Now insert all of the callee saves
    for (function_index, block_range) in function_block_ranges.iter().copied().enumerate() {
        let callee_saves = MemBinding::CalleeSaves(function_index);
        for (lt, block) in all[block_range.as_range()].iter_mut().zip(&ir[block_range]) {
            lt.ordered_by_start.insert(0, callee_saves);
            lt.binding_starts.insert(callee_saves, 0);
            lt.binding_ends.insert(callee_saves, block.statements.len());
        }
    }

    tracing::trace!(target: "lifetimes::mem", "gathered lifetimes: {:#?}", all);

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

#[derive(Debug, Hash, Eq, PartialEq, PartialOrd, Clone, Copy)]
pub enum MemBinding {
    CalleeSaves(usize),
    IR(Binding),
}

impl fmt::Display for MemBinding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemBinding::CalleeSaves(func) => write!(f, "[callee saves for @{func}]"),
            MemBinding::IR(b) => b.fmt(f),
        }
    }
}
