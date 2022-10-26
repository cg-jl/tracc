//! Flag allocator
//! This one's simple, since flags are immutable.
//! A binding is suitable to be in a flag if:
//!     - it comes from a `cmp` statement
//!     - it is not used elsewhere

use crate::intermediate::analysis;
use std::collections::HashMap;

use crate::{
    codegen::assembly::Condition,
    intermediate::{Binding, Statement, Value, IR},
};

#[allow(clippy::needless_lifetimes)] // I prefer an explicit lifetime here.
pub fn get_used_flags<'code>(
    ir: &'code IR,
) -> impl Iterator<Item = (Binding, Condition)> + 'code {
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
    })
}
