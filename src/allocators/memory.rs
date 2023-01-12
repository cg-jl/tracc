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
                .position(|other| lifetime.binding_ends[other] > start)
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
    // memory lifetimes are more tricky because memory bindings have 'multiple' starts and ends.
    // Since the allocator accounts for bindings that it has already allocated (those just
    // contribute to the free blocks), we can push multiple starts and ends per memory. We'll also
    // have to take into account the lifetimes that pass through blocks without being used
    // explicitly inside them.
    let mut all = {
        let mut all = Box::new_uninit_slice(ir.code.len());
        all.iter_mut().enumerate().for_each(|(i, m)| {
            m.write(BlockLifetimes {
                block_index: i,
                ordered_by_start: Default::default(),
                binding_starts: Default::default(),
                binding_ends: Default::default(),
            });
        });
        unsafe { all.assume_init() }
    };

    // 0. Set up all functions
    for (function_index, block_range) in function_block_ranges.iter().copied().enumerate() {
        let callee_saves = MemBinding::CalleeSaves(function_index);
        for (lt, block) in all[block_range.as_range()].iter_mut().zip(&ir[block_range]) {
            lt.ordered_by_start.push(callee_saves);
            lt.binding_starts.insert(callee_saves, 0);
            lt.binding_ends.insert(callee_saves, block.statements.len());
        }
    }

    let mut binding_start_blocks = HashMap::new();

    // 1. Gather all requested binding starts
    for (lt, (bb, block)) in all
        .iter_mut()
        .zip(analysis::iterate_with_bindings(&ir.code))
    {
        for (stmt_index, statement) in block.statements.iter().enumerate() {
            if let Statement::Assign { index, .. } = statement {
                if allocations_needed.contains_key(index) {
                    let binding = MemBinding::IR(*index);
                    lt.ordered_by_start.push(binding);
                    lt.binding_starts.insert(binding, stmt_index);
                    binding_start_blocks.insert(*index, bb);
                }
            }
        }
    }

    let mut binding_end_blocks: HashMap<_, HashSet<_>> = HashMap::new();

    // 2. Gather all requested binding ends.
    for block in analysis::BottomTopTraversal::from(ir) {
        for (stmt_index, statement) in ir[block].statements.iter().enumerate().rev() {
            match statement {
                Statement::Store { mem_binding, .. }
                | Statement::Assign {
                    value: Value::Load { mem_binding, .. },
                    ..
                } => {
                    if allocations_needed.contains_key(mem_binding)
                        && binding_end_blocks
                            .entry(*mem_binding)
                            .or_default()
                            .insert(block)
                    {
                        all[block.0]
                            .binding_ends
                            .insert(MemBinding::IR(*mem_binding), stmt_index);
                    }
                }
                _ => (),
            }
        }
    }

    // 3. Now that we've got starts and ends, we have to fill in the 'middle' blocks.
    // We'll do this safely by starting in the end block and only processing the branches that lead to the start block.
    let indirect_parents = analysis::fill_indirect_parents(ir);

    tracing::trace!(target: "alloc::memory", "indirect parents: {indirect_parents:#?}");
    tracing::trace!(target: "alloc::memory", "backwards graph: {:#?}", &ir.backwards_map);

    for (binding, end_block, start_block) in binding_end_blocks
        .into_iter()
        .flat_map(|(binding, end_blocks)| {
            let start_block = *binding_start_blocks.get(&binding).unwrap();
            end_blocks
                .into_iter()
                .map(move |end| (binding, end, start_block))
        })
        .filter(|(_, end, start)| start != end)
    {
        let mem_binding = MemBinding::IR(binding);
        all[start_block.0]
            .binding_ends
            .insert(mem_binding, ir[start_block].statements.len());
        for middle_block in analysis::antecessors_filtering_branches(ir, end_block, |b| {
            indirect_parents[&b].contains(&start_block)
        }) {
            let pushed_as_start = all[middle_block.0]
                .binding_ends
                .insert(mem_binding, ir[middle_block].statements.len())
                .is_none();
            let pushed_as_end = all[middle_block.0]
                .binding_starts
                .insert(mem_binding, 0)
                .is_none();
            if pushed_as_end || pushed_as_start {
                all[middle_block.0].ordered_by_start.push(mem_binding);
            }
        }
    }

    for lifetimes in all.iter_mut() {
        // NOTE: we need stable sort so that things are kept in insertion order.
        // when ordering, make sure to put bindings that are from blocks upper in the chain
        // before bindings that come after, so compare their starting blocks before comparing their
        // local position.
        lifetimes
            .ordered_by_start
            .sort_unstable_by(|a, b| match (a, b) {
                (MemBinding::CalleeSaves(_), MemBinding::IR(_)) => core::cmp::Ordering::Less,
                (MemBinding::IR(_), MemBinding::CalleeSaves(_)) => core::cmp::Ordering::Greater,
                (ma @ MemBinding::IR(a), mb @ MemBinding::IR(b)) => {
                    match binding_start_blocks[a].cmp(&binding_start_blocks[b]) {
                        core::cmp::Ordering::Equal => {
                            lifetimes.binding_starts[ma].cmp(&lifetimes.binding_starts[mb])
                        }
                        other => other,
                    }
                }
                (MemBinding::CalleeSaves(_), MemBinding::CalleeSaves(_)) => unreachable!(
                    "what the fuck are two callee saves for different functions doing here?!"
                ),
            });
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
