//! Register analysis of the code

use crate::intermediate::{Binding, IR};

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
// #2. Take the hints and assign the register indices.
//     allocate memory for mid-saving them, if necessary (accounting for their size)
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
    /// The binding is used in a phi function: we'd want it to have the same register as the other bindings
    Phi(Vec<Binding>),
}


pub fn debug_what_im_doing(ir: &IR) {
    dbg!(ir);
}
