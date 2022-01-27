use std::collections::HashMap;

use super::{
    refactor::{self, redefine::Rename},
    BlockBinding, IRCode, Statement, Value, IR,
};

pub fn prepare_for_codegen(ir: &mut IR) {
    remove_aliases(&mut ir.code);
    prune_unreached_blocks(ir);
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
            for branch_target in ir.forward_map.remove(&next).into_iter().flatten() {
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
        let removal_index = unused_binding.0;
        // #1. Rename references to the blocks after the current block to match their new index
        // (drops by 1)
        for further_index in removal_index + 1..ir.code.len() {
            let target_index = further_index - 1;
            let old = BlockBinding(further_index);
            let new = BlockBinding(target_index);

            refactor::rename_block(ir, old, new);
        }

        // #2. Now we can safely remove the block
        ir.code.remove(removal_index);
    }
}

///// Block aliasing occurs when a block only has a single branch as its code
// fn remove_block_aliasing(ir: &mut IR) {
//     // collect everything into a vec to end the ref
//     let aliases: Vec<_> = ir
//         .code
//         .iter()
//         .enumerate()
//         .filter_map(|(block_index, block)| {
//             // single branch
//             if block.statements.is_empty() {
//                 // jumps directly to a block
//                 if let BlockEnd::Branch(Branch::Unconditional { target }) = block.end {
//                     Some((BlockBinding(block_index), target))
//                 } else {
//                     None
//                 }
//             } else {
//                 None
//             }
//         })
//         .collect();
//
//     for (aliased_block, alias_target) in dbg!(aliases) {
//         // #1. Patch the code
//         rename_block(ir, aliased_block, alias_target);
//         // #2. Patch the backwards map:
//         // move the sources from `aliased_block` to `alias_target`
//         if let Some(sources) = ir.backwards_map.remove(&aliased_block) {
//             let target_sources = ir
//                 .backwards_map
//                 .get_mut(&alias_target)
//                 .expect("Malformed backwards map: missing back reference to already detected jump");
//             for source in sources {
//                 if !target_sources.contains(&source) {
//                     target_sources.push(source);
//                 }
//             }
//         }
//     }
// }
