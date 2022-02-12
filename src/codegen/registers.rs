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

// NOTE: after figuring out the registers each one has, we might need to allocate some other
// bindings for spills. why don't we 'tweak' the memory module to run after this and receive a set
// of sizes with their lifetimes and we manually assign those later to bindings/spills?
//
// Also we'll want to spit out where the spills go, and an intelligent allocator that reserves
// callee-saved registers for call-usages only, as possible

// where does each binding die?

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
