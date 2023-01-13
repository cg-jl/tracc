//! Flag allocator
//! This one's simple, since flags are immutable.
//! A binding is suitable to be in a flag if:
//!     - it comes from a `cmp` statement
//!     - it is not used elsewhere

use crate::ir::analysis;
use std::collections::HashMap;

use crate::{
    asmgen::assembly::Condition,
    ir::{Binding, Statement, Value, IR},
};

// We need a way to mark where certain "knowns" are invalidated, e.g "eq" will be invalidated in
// some compares but not in others. Once we know that those are invalidated, we'll only need to check
// that through the binding's lifetime inside the block (i.e from its definition to its use) there
// is no invalidation of its comparison result. This way we can keep using a condition even if it's
// not inside a register.
//
// The other problem that arises for this "parallelism" to work is that we have to track where
// flags have a "flag-binding" duality, where they are used as values to e.g add (needs a register
// allocated to it) or they're just used in a branch.

#[allow(clippy::needless_lifetimes)] // I prefer an explicit lifetime here.
pub fn get_used_flags<'code>(ir: &'code IR) -> impl Iterator<Item = (Binding, Condition)> + 'code {
    ir.code
        .iter()
        .flat_map(|block| {
            block.statements.iter().filter_map(|statement| {
                if let Statement::Assign {
                    index: target,
                    value: Value::Cmp { condition, .. },
                } = statement
                {
                    Some((*target, *condition))
                } else {
                    None
                }
            })
        })
        .filter(|(binding, _)| is_binding_usable_for_flag(*binding, ir))
}

fn is_binding_usable_for_flag(binding: Binding, ir: &IR) -> bool {
    // check usages for the binding
    // and check that it isn't used in any statement besides a branch
    // XXX: sequential `cmp` statements might also be a candidate here.
    use analysis::BindingUsage;
    !ir.code.iter().any(|block| {
        block
            .statements
            .iter()
            .any(|statement| statement.contains_binding(binding))
            || matches!(block.end, crate::ir::BlockEnd::Return(x) if x == binding)
    })
}
