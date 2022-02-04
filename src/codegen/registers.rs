//! Register analysis of the code

use std::collections::{HashMap, HashSet};

use crate::intermediate::{
    analysis, Binding, BlockBinding, BlockEnd, Branch, Statement, Value, IR,
};

use super::assembly::Condition;

// #1. Get what each binding is used for, call it hints:
//      - returning from a function:
//          - the binding is used in a `ret` statement
//      - phi node:
//          - the binding is used in a phi node with other bindings
//      - has a call before it's used
//          - a map of bindings that have not yet met their definition will be available
//          - when encountering a call, all the bindings in the map will receive that hint
//      - is an argument to a call
//          - we might want to compute the value in its corresponding register
//
// #2. Take the hints and assign the registers.
//     allocate memory for mid-saving them, if necessary (accounting for their size)
//     note that bindings that were allocated by the memory module will be given to the register
//     allocator so that it knows it mustn't allocate a general purpose register for those (they are already
//     assigned to `sp`)
//
// #3. Check for callee-saved registers (might need extra memory for those)
//     and reserve full-register size per each
//
// #4. Check whether any call is made. If that is the case, annotate that for the
//     generator so it can save the frame and stack pointer from the calling routine
//     (a64 ABI)
//
// #5. Give back a register map for each binding, the extra memory it needs, and
//     annotations like register pre-post saving (block & statement spans).
//     Extra memory will be given in size directly and will need to be aligned to 4 bytes
//     (the stack needs everything aligned to at least 4 bytes)

#[derive(Debug)]
enum Hint {
    /// The binding is used in a `ret` statement
    Return,
    /// The binding is used to branch to different blocks
    CmpResult(Condition),
    /// The binding is used in a phi function to other binding
    Phi { target: Binding },
    /// The binding is a pointer to a stack-allocated frame, ignore it
    AllocPtr,
}

#[derive(Debug, Clone, Copy)]
enum Usage {
    /// the binding is used in a computation of other binding
    Binding(Binding),
    /// the binding is used in a store
    Store(Binding),
    /// the binding is used in a return statement
    Return,
    /// the binding is used as a switch for branch
    Branch,
}
type UsageMap = HashMap<Binding, Vec<Usage>>;

// get what each binding depends on
fn get_usage_map(ir: &IR) -> UsageMap {
    let mut usage_map = UsageMap::new();

    for block in &ir.code {
        for statement in &block.statements {
            match statement {
                Statement::Store {
                    mem_binding,
                    binding,
                    ..
                } => usage_map
                    .entry(*binding)
                    .or_default()
                    .push(Usage::Store(*mem_binding)),
                Statement::Assign { index, value } => {
                    use analysis::BindingUsage;
                    for dep in value.binding_deps() {
                        usage_map
                            .entry(dep)
                            .or_default()
                            .push(Usage::Binding(*index));
                    }
                }
            }
        }

        match block.end {
            BlockEnd::Return(ret) => usage_map.entry(ret).or_default().push(Usage::Return),
            BlockEnd::Branch(Branch::Conditional { flag, .. }) => {
                usage_map.entry(flag).or_default().push(Usage::Branch)
            }
            BlockEnd::Branch(Branch::Unconditional { .. }) => {}
        }
    }
    usage_map
}

//fn uses_from_deps(deps: &HashMap<Binding, Vec<Binding>>) -> UsageMap {}

// TODO: figure out how cpu status flags are affected by each binding and if the last modifier to
// the flags was the same binding that is indicating a `CmpResult`, we can avoid allocating a
// register for it. If any of the binding's uses requires a register (or discards the flags) then
// the flag binding will be used on a register and re-checked in case of a branch where the state
// was discarded.
//
// We have 3 types of bindings:
//  - registers
//  - cpu state flags
//  - stack pointer bindings (with a known offset)

pub fn debug_what_im_doing(ir: &IR) {
    let usage_map = get_usage_map(ir);
    dbg!(ir, usage_map);
}
