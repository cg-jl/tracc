use std::collections::HashMap;
use std::collections::HashSet;

use super::{
    refactor::{self, redefine::Rename},
    BasicBlock, Binding, BlockBinding, IRCode, Statement, Value, IR,
};

pub fn run_cleanup(ir: &mut IR) {
    prune_unreached_blocks(ir);
    remove_aliases(&mut ir.code);
    remove_unused_bindings(ir);
}

fn remove_unused_bindings(ir: &mut IR) {
    // #1. Catch all the definitions

    use super::analysis::lifetimes::BlockAddress;
    let defs_map: HashMap<_, _> = super::analysis::lifetimes::get_defs(ir).collect();

    let defs: HashSet<_> = defs_map.keys().cloned().collect();

    // #2 Catch all the binding dependencies

    use super::analysis::BindingUsage;
    let deps: HashSet<Binding> = ir
        .code
        .iter()
        .flat_map(|block: &BasicBlock| block.binding_deps())
        .collect();

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
        indices.sort_unstable();

        for index in indices.into_iter().rev() {
            ir[block].statements.remove(index);
        }
    }
}

/// final cleanup before codegen: Remove all aliasing behavior
fn remove_aliases(code: &mut IRCode) {
    // #1. Catch all the aliases
    let mut aliases = HashMap::new();

    for (block_index, block) in code.iter().enumerate() {
        for (statement_index, statement) in block.statements.iter().enumerate() {
            if let Statement::Assign {
                index: target,
                value: Value::Binding(other),
            } = statement
            {
                aliases.insert(*target, (*other, block_index, statement_index));
            }
        }
    }

    // #2. Rebind aliases
    for (from, (to, block_index, statement_index)) in aliases {
        code.rename(from, to); // rebind
        debug_assert_eq!(
            code[block_index].statements.remove(statement_index),
            Statement::Assign {
                index: from,
                value: Value::Binding(to)
            },
            "Health check: remove alias correctly"
        );
    }
}

/// prune not reached blocks
fn prune_unreached_blocks(ir: &mut IR) {
    // #1. Walk the CFG and prune unreached blocks from the map
    let unused_blocks = {
        let mut unreached = Vec::new();

        // start by getting the 'not even registered' blocks.
        let mut queue: Vec<_> = (1..ir.code.len())
            .map(BlockBinding)
            .filter(|binding| !ir.backwards_map.contains_key(binding))
            .collect();

        // now propagate to their children
        while !queue.is_empty() {
            let next = queue.pop().unwrap();
            if unreached.contains(&next) {
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

    // for all unused blocks:
    for unused_binding in unused_blocks {
        // remove the block
        // UNSAFE: safe. the block is no longer used.
        unsafe { refactor::remove_block(ir, unused_binding) };
    }
}
