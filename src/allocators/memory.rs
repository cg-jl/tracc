//! First step of code generation is to get the stack memory usage for the code

use std::collections::{HashMap, HashSet};

use crate::intermediate::{BasicBlock, Binding, BlockBinding, Statement, Value, IR};

use crate::codegen::assembly;
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
use crate::intermediate::analysis;

type AllocMap = HashMap<Binding, usize>;

pub fn figure_out_allocations(
    ir: &IR,
    allocations_needed: AllocMap,
    lifetime_collisions: &analysis::lifetimes::CollisionMap,
) -> (MemoryMap, usize) {
    log::debug!("requested allocations: {allocations_needed:?}");

    let collision_map = compute_memory_collisions(ir);

    log::debug!("allocation collisions: {collision_map:?}");
    log::debug!("lifetime collisions: {lifetime_collisions:?}");

    let mut local_collisions: Vec<(_, HashSet<_>)> = collision_map
        .iter()
        .chain(lifetime_collisions.into_iter())
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

// tracks per dependency where it is loaded.
pub type LoadDependencies = HashMap<Binding, HashSet<analysis::lifetimes::BlockAddress>>;

fn find_load_dependencies<'c>(
    ir: &IR,
    binding: Binding,
    cache: &'c mut HashMap<Binding, LoadDependencies>,
) -> &'c LoadDependencies {
    use analysis::BindingUsage;

    // NOTE: it's ok to make a copy of the reference here since the exclusivity is proven
    // by means of control flow: There's no parallel thing going on.
    let cache_ref_copy = unsafe {
        let ptr = cache as *mut HashMap<_, _>;
        ptr.as_mut().unwrap_unchecked()
    };

    if let Some(cached) = cache.get(&binding) {
        return cached;
    }

    let mut deps = LoadDependencies::new();
    let (value, address) = analysis::find_assignment_value_with_adress(&ir.code, binding).unwrap();

    if let Value::Load { mem_binding, .. } = value {
        deps.entry(*mem_binding).or_default().insert(address);
    }

    value.visit_value_bindings(&mut |dep| {
        deps.extend(
            find_load_dependencies(ir, dep, cache_ref_copy)
                .iter()
                .map(|(x, y)| (*x, y.clone())),
        );
        std::ops::ControlFlow::<(), ()>::Continue(())
    });

    cache_ref_copy.insert(binding, deps);
    // SAFE: we've just inserted it.
    unsafe { cache.get(&binding).unwrap_unchecked() }
}

fn compute_memory_collisions(ir: &IR) -> HashMap<Binding, HashSet<Binding>> {
    // Find stores. For each of the stores, use their direct dependencies to map back to a load.
    use analysis::BindingUsage;
    let mut deps_cache = HashMap::new();
    let mut collisions = HashMap::new();

    analysis::statements_with_addresses(ir)
        .filter_map(|(statement, addr)| {
            if let Statement::Store {
                mem_binding,
                binding,
                byte_size: _,
            } = statement
            {
                Some((*mem_binding, *binding, addr))
            } else {
                None
            }
        })
        .for_each(|(stored_mem, stored_value, store_location)| {
            let collision_set: &mut HashSet<_> = collisions.entry(stored_mem).or_default();

            for (&load_dep, load_locations) in
                find_load_dependencies(ir, stored_value, &mut deps_cache)
            {
                if load_dep == stored_mem {
                    continue;
                }
                if load_locations
                    .iter()
                    .any(|loc| loc.happens_before(ir, store_location))
                {
                    collision_set.insert(load_dep);
                }
            }
        });

    collisions
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
