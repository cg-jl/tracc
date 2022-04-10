use std::collections::HashMap;

use crate::{
    codegen::assembly::Condition,
    intermediate::{
        analysis::{self, lifetimes::BlockAddress},
        Binding, BlockEnd, Branch, Statement, Value, IR,
    },
};

// NOTE: let's,for now, suppose that all places where flags are generated are not
// touched by anyone else. So we just have to annotate the bindings with having that condition
// flag.
//
// What the codegen will do is check the condition and canonicalize it for eq, lt, le.
// Then a condition can be outputted for each flag.
// NOTE: if the flag was in a register, the address of the label to branch might be computed
// instead of the actual flag.

// allocates CPU status flags for some bindings
// spills are put into registers

// the main function here should get a usage map and
// try allocating cpu flags to bindings that can benefit of just being flags (no registers).
// bindings that contained flags that are spilled are returned back with the CPU flag they
// represent.

use bitflags::bitflags;

bitflags! {
    pub struct ConditionFlags: u16 {
        /// N for Negative
        const N = 0b1;
        /// Z for Zero
        const Z = 0b10;
        /// C for Carry
        const C = 0b100;
        /// V for oVerflow
        const V = 0b1000;
    }
    // note: mrs <reg>, NZCV to know about flags / save the flags
    //       msr NZCV, <reg> to set the flags in that register
}

impl ConditionFlags {
    // returns a (canonical) condition for the given flag
    pub fn to_asm_condition(self) -> Condition {
        match (self.contains(Self::N), self.contains(Self::Z)) {
            (true, true) => Condition::LessEqual,
            (true, false) => Condition::LessThan,
            (false, true) => Condition::Equals,
            _ => unreachable!("unknown condition for flag: {:?}", self),
        }
    }
}

// #1. Get which bindings/instructions use which flags for whatever reason
// #2. Get `cmp` operations that will touch the flags (the rest is decided by us whether to use
// ALU flags or not)

// currently I won't think of overflowing_add and so on. Just

pub fn get_flag_usages(ir: &IR) -> HashMap<Binding, Vec<BlockAddress>> {
    let mut usages = HashMap::<_, Vec<_>>::new();
    for (blockid, block) in analysis::iterate_with_bindings(&ir.code) {
        for (statement_id, statement) in block.statements.iter().enumerate() {
            use analysis::BindingUsage;
            for dep in statement.binding_deps().into_iter().filter(|dep| {
                matches!(
                    analysis::find_assignment_value(&ir.code, *dep).expect("undefined binding"),
                    Value::Cmp { .. }
                )
            }) {
                usages.entry(dep).or_default().push(BlockAddress {
                    block: blockid,
                    statement: statement_id,
                });
            }
        }

        if let BlockEnd::Branch(Branch::Conditional { flag, .. }) = block.end {
            if let Value::Cmp { .. } = analysis::find_assignment_value(&ir.code, flag).unwrap() {
                usages.entry(flag).or_default().push(BlockAddress {
                    block: blockid,
                    statement: block.statements.len(),
                });
            }
        }
    }
    usages
}

pub fn get_flag_sources(ir: &IR) -> HashMap<Binding, ConditionFlags> {
    ir.code
        .iter()
        .flat_map(|block| {
            block.statements.iter().filter_map(|statement| {
                if let Statement::Assign {
                    index,
                    value: Value::Cmp { condition, .. },
                } = statement
                {
                    Some((*index, get_flags(*condition)))
                } else {
                    None
                }
            })
        })
        .collect()
}

fn get_flags(condition: Condition) -> ConditionFlags {
    match condition {
        Condition::Equals => ConditionFlags::Z,
        Condition::LessThan => ConditionFlags::N,
        Condition::LessEqual => ConditionFlags::N | ConditionFlags::Z,
        // XXX: should I remove the existence of these last three with a simple pass?
        Condition::NotEquals => ConditionFlags::N,
        Condition::GreaterThan => ConditionFlags::N,
        Condition::GreaterEqual => ConditionFlags::N | ConditionFlags::Z,
    }
}
