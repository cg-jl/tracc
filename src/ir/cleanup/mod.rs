use std::collections::HashMap;
use std::collections::HashSet;

use super::PhiDescriptor;
use super::{
    refactor::{self, redefine::Rename},
    BasicBlock, Binding, BlockBinding, BlockEnd, Branch, IRCode, Statement, Value, IR,
};

pub struct CleanupResult {
    /// Whether the cleanup removed aliases
    pub renamed_bindings: bool,
    pub rewrote_phis: bool,
    pub inlined_blocks: bool,
}

pub fn run_safe_cleanup(ir: &mut IR) -> CleanupResult {
    let rewrote_phis = rewrite_nonunique_phis(ir);
    let alias = remove_aliases(&mut ir.code);
    remove_unused_bindings(ir);
    prune_unreached_blocks(ir);
    let inlined_blocks = remove_redundant_jumps(ir);
    CleanupResult {
        renamed_bindings: alias,
        rewrote_phis,
        inlined_blocks,
    }
}

pub fn remove_unused_bindings(ir: &mut IR) {
    // #1. Catch all the definitions

    use super::analysis::lifetimes::BlockAddress;
    let defs_map: HashMap<_, _> = super::analysis::lifetimes::get_defs(ir).collect();

    let defs: HashSet<_> = defs_map.keys().cloned().collect();

    // #2 Catch all the binding dependencies

    use super::analysis::BindingUsage;

    let mut deps = HashSet::new();
    ir.visit_value_bindings(&mut |dep| {
        deps.insert(dep);
        std::ops::ControlFlow::<(), ()>::Continue(())
    });

    // #3. Anything defined but not depended on is unused
    let unused = defs
        .difference(&deps)
        .cloned()
        .map(|binding| defs_map[&binding]);

    // #4. Organize the addresses into their corresponding blocks
    let mut blocks: HashMap<BlockBinding, Vec<usize>> = HashMap::new();
    for BlockAddress { block, statement } in unused {
        blocks.entry(block).or_default().push(statement);
    }

    // #5. Do the liberations
    for (block, mut indices) in blocks {
        // sort the indices so that deletion can be done while keeping all indices correct
        indices.sort_unstable_by(|a, b| a.cmp(b).reverse());

        for index in indices.into_iter() {
            ir[block].statements.remove(index);
        }
    }
}

fn order_by_dependency<K: Copy + Eq + std::hash::Hash>(mut map: HashMap<K, K>) -> Vec<(K, K)> {
    let mut v = Vec::new();
    while let Some(k) = map.keys().find(|k| !map.contains_key(&map[k])).copied() {
        let value = map.remove(&k).unwrap();
        v.push((k, value))
    }
    v
}

/// More efficient routine when it is known that the expected aliases are in the same block.
pub fn remove_aliases_in_same_block(block: &mut BasicBlock) {
    let aliases = block
        .statements
        .iter()
        .filter_map(|statement| {
            if let Statement::Assign {
                index: target,
                value: Value::Binding(bind),
            } = statement
            {
                Some((*target, *bind))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();

    let mut to_remove = HashSet::new();

    for (from, to) in order_by_dependency(aliases) {
        to_remove.insert(
            block
                .statements
                .iter()
                .enumerate()
                .find_map(|(index, stmt)| {
                    if let Statement::Assign { index: target, .. } = stmt {
                        if target == &from {
                            Some(index)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .unwrap(),
        );
        block.rename(from, to);
    }
    let mut to_remove = to_remove.into_iter().collect::<Vec<_>>();
    to_remove.sort_unstable();
    for index in to_remove.into_iter().rev() {
        block.statements.remove(index);
    }
}

// Make all phi nodes standard:
// - remove any arguments that are not their direct parents
// - remove any phi nodes that point to a single binding/ single argument.
// - remove any empty phi nodes, replacing them with uninit.
pub fn rewrite_nonunique_phis(ir: &mut IR) -> bool {
    tracing::debug!(target: "irshow::cleanup::nonunique_phis", "ir: {ir:?}");

    let mut rewrote_phis = false;

    for (bb, block) in ir.code.iter_mut().enumerate() {
        // Either a root or unreachable
        let Some(parents) = ir.backwards_map.get(&BlockBinding(bb)) else {
            continue;
        };

        for value in refactor::values_mut(block) {
            if let Value::Phi { nodes } = value {
                rewrote_phis = true;
                let mut nodes = std::mem::take(nodes);

                nodes.retain(|phi| parents.contains(&phi.block_from));

                std::mem::replace(value, recount_phi(nodes));
            }
        }
    }

    rewrote_phis
}

pub fn remove_aliases(code: &mut IRCode) -> bool {
    tracing::debug!(target: "cleanup::alias", "code: {code:?}");
    // #1. Catch all the aliases
    let mut aliases = Vec::new();

    for (block_index, block) in code.iter().enumerate() {
        for (statement_index, statement) in block.statements.iter().enumerate() {
            if let Statement::Assign {
                index: target,
                value: Value::Binding(other),
            } = statement
            {
                aliases.push((*target, *other));
            }
        }
    }

    tracing::debug!(target: "cleanup::alias", "found aliases: {aliases:?}");

    let had_aliases = !aliases.is_empty();

    // #2. Rebind aliases
    for (from, to) in aliases {
        code.rename(from, to); // rebind
    }
    had_aliases
}

/// Removes blocks that just serve to redirect a jump, which might have resulted from compiling
/// `break`/`continue` statements, which have some redundant redirection built into them.
pub fn remove_redundant_jumps(ir: &mut IR) -> bool {
    let mut inlined_blocks = false;
    for b in (0..ir.code.len()).rev().map(BlockBinding) {
        {
            if ir[b].statements.is_empty() {
                if let BlockEnd::Branch(Branch::Unconditional { target }) = ir[b].end {
                    // only those who don't make an endless loop into themselves.
                    if target != b {
                        tracing::debug!(target: "cleanup", "found redundant jumping block {b} -> {target}");
                        inlined_blocks = true;
                        // HACK: When calling this from redundant jump optimization,
                        // we end up with an incorrect backwards map,
                        // but most of the time it doesn't matter since we're removing the faulty block.
                        // The mishap is that what we want is to merge the antecessors of `b` with
                        // the antecessors of `target`. Since `b` has no code, any phi nodes that
                        // pointed to `b` should have already been taken care of by now.
                        unsafe {
                            refactor::rename_block(ir, b, target);
                        }
                    }
                }
            }
        }
    }

    if inlined_blocks {
        // re-compute the branching graph
        let (forward_map, backwards_map) = super::generate::generate_branching_graphs(&ir.code);
        ir.forward_map = forward_map;
        ir.backwards_map = backwards_map;
    }
    inlined_blocks
}

pub fn recount_phi(nodes: Vec<PhiDescriptor>) -> Value {
    match nodes.len() {
        0 => Value::Uninit,
        1 => Value::Binding(nodes[0].value),
        _ => {
            let first = nodes[0].value;
            if nodes[1..].iter().all(|ph| ph.value == first) {
                Value::Binding(first)
            } else {
                Value::Phi { nodes }
            }
        }
    }
}

/// prune not reached blocks
pub fn prune_unreached_blocks(ir: &mut IR) {
    tracing::debug!(target: "cleanup::prune-unreached", "IR: {ir:?}");
    // #1. Walk the CFG and prune unreached blocks from the map
    let unused_blocks = {
        let mut unreached = Vec::new();

        // start by getting the 'not even registered' blocks.
        let mut queue: Vec<_> = (0..ir.code.len())
            .map(BlockBinding)
            .filter(|x| !ir.function_entrypoints.contains(x))
            .filter(|binding| !ir.backwards_map.contains_key(binding))
            .collect();

        // now propagate to their children
        while !queue.is_empty() {
            let next = queue.pop().unwrap();
            if unreached.contains(&next) || ir.function_entrypoints.contains(&next) {
                continue;
            }
            unreached.push(next);

            // for the branches it may have, remove this block from its parents list
            // and if it ends up empty then add it to the queue
            for branch_target in ir
                .forward_map
                .remove(&next)
                .into_iter()
                .flatten()
                .filter(|binding| binding.0 != 0)
            {
                // #1. Remove its parent
                let parents = ir
                    .backwards_map
                    .get_mut(&branch_target)
                    .expect("Malformed backwards graph: no parents assign for a known child");

                parents.retain(|x| x != &next);

                // #2. If the parents are empty, remove the backwards map entry and add it to the
                // queue
                if parents.is_empty() {
                    ir.backwards_map.remove(&branch_target);
                    queue.push(branch_target);
                }
            }
        }

        unreached.sort_unstable();
        unreached.reverse();
        unreached
    };

    tracing::debug!(target: "cleanup", "found unused blocks: {unused_blocks:?}");

    // for all unused blocks:
    for unused_binding in unused_blocks {
        // SAFE: the block is proven to be unreachable.
        unsafe { refactor::remove_block_from_phis(ir, unused_binding) };
        // remove the block
        // UNSAFE: safe. the block is no longer used.
        unsafe { refactor::remove_block(ir, unused_binding) };
    }

    // re-compute the branching graph
    let (forward_map, backwards_map) = super::generate::generate_branching_graphs(&ir.code);
    ir.forward_map = forward_map;
    ir.backwards_map = backwards_map;
}
