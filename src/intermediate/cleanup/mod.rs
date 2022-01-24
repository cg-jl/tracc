use std::collections::HashMap;

use crate::intermediate::{cleanup::redefine::Rename, Statement, Value};

use super::BackwardsMap;
use super::BlockBinding;
use super::IRCode;
use super::IR;

mod redefine;

pub fn perform_cleanup(ir: &mut IR) {
    remove_aliases(&mut ir.code);
    prune_unreached_blocks(&ir.backwards_map, &mut ir.code);
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
fn prune_unreached_blocks(backwards_map: &BackwardsMap, code: &mut IRCode) {
    // list of unused block indices can have up to length of code -1 since the first block (root
    // block) is always used
    let mut unused_blocks = Vec::with_capacity(code.len() - 1);
    for i in 1..code.len() {
        // no other block reaches this one
        if backwards_map.get(&BlockBinding(i)).is_none() {
            unused_blocks.push(i);
        }
    }

    unused_blocks.sort_unstable(); // don't care of order of equal elements... there are none equal elements.
    for offsetted_index in unused_blocks
        .into_iter()
        .rev()
        .enumerate()
        .map(|(offset, index)| index - offset)
    {
        code.remove(offsetted_index);
    }
}
