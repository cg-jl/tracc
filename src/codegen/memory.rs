//! First step of code generation is to get the stack memory usage for the code

use std::collections::{HashMap, HashSet};

use crate::intermediate::{Binding, BlockEnd, IRCode, Statement, Value};

/// Allocation info for a given block
#[derive(Debug)]
pub struct AllocInfo {
    pub alloc_size: usize,
    pub alloc_map: HashMap<Binding, usize>,
}

// TODO: I think I need to figure out register usage for the block first, then figure out the
// memory I need.

/// Traverse the CFG to know how much memory we need at most and
/// memory map for each block
pub fn resolve_memory(code: &IRCode) -> (usize, Vec<AllocInfo>) {
    // #1. get the memory usage map
    let mut mem_usage = Vec::with_capacity(code.len());
    mem_usage.extend(memories_per_block(code));

    // #2. Get the memory scores and sort them
    let score = memories_score(code, &mem_usage);

    // #3. Make the allocation map
    let allocation_sizes: HashMap<_, _> = score
        .keys()
        .map(|mem| {
            // TODO: make this use an external search function
            let alloc_size = find_allocation_size(code, *mem)
                .expect("could not find allocation size for memory binding");
            (*mem, alloc_size)
        })
        .collect();

    // #4. Actually allocate the memory needed
    mem_usage
        .into_iter()
        .map(|mut usage_list| {
            // we need reversed comparisons so highest usefulness score is put first
            usage_list.sort_unstable_by(|a, b| score[b].cmp(&score[a]));
            allocate_block_memory(usage_list, &allocation_sizes)
        })
        .fold(
            (0, Vec::with_capacity(code.len())),
            |(max_size, mut allocs), (alloc_size, alloc_map)| {
                allocs.push(AllocInfo {
                    alloc_size,
                    alloc_map,
                });
                (max_size.max(alloc_size), allocs)
            },
        )
}

/// Allocate the stack memory that a block will need for `alloca` objects (registers don't count
/// yet)
fn allocate_block_memory(
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

/// Analyze the memories that each block of the code uses, either by store or load
fn memories_per_block(code: &IRCode) -> impl Iterator<Item = Vec<Binding>> + '_ {
    code.iter().map(|block| {
        let mut bindings = Vec::new();
        for statement in &block.statements {
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
    })
}

/// Get a usefulness score for each of the memories,
/// being the score higher the more flow it occupies.
///
/// Usefulness score is computed by following through the CFG in a dfs manner,
/// adding one each time a new block is encountered and subtracting one each time an already
/// visited block is encountered. This is done to avoid a branching flow influencing the score, by
/// making all the branches that converge in the same area score once.
fn memories_score(code: &IRCode, usage_per_block: &[Vec<Binding>]) -> HashMap<Binding, usize> {
    let mut score: HashMap<_, usize> = HashMap::new();

    let mut visited = HashSet::new();

    let mut queue = vec![0];

    while !queue.is_empty() {
        // UNSAFE: queue is not empty.
        let next = unsafe { queue.pop().unwrap_unchecked() };
        if visited.contains(&next) {
            for mem in &usage_per_block[next] {
                score.entry(*mem).and_modify(|x| *x -= 1);
            }
        } else {
            for mem in &usage_per_block[next] {
                score.entry(*mem).and_modify(|x| *x += 1).or_insert(1);
            }
            visited.insert(next);
            if let BlockEnd::Branch(branch) = code[next].end {
                match branch {
                    crate::intermediate::Branch::Unconditional { target } => queue.push(target.0),
                    crate::intermediate::Branch::Conditional {
                        flag: _,
                        target_true,
                        target_false,
                    } => {
                        queue.push(target_true.0);
                        queue.push(target_false.0);
                    }
                }
            }
        }
    }

    score
}

/// Find the allocation size for a particular memory binding
fn find_allocation_size(code: &IRCode, target_alloc: Binding) -> Option<usize> {
    super::find_definition_in_code(code, target_alloc).and_then(|(block_index, statement_index)| {
        if let Statement::Assign {
            value: Value::Allocate { size },
            ..
        } = code[block_index].statements[statement_index]
        {
            Some(size)
        } else {
            None
        }
    })
}

fn align(value: usize, align_to: usize) -> usize {
    align_to * (value / align_to) + if value % align_to == 0 { 0 } else { align_to }
}
