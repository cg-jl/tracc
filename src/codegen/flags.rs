// allocates CPU status flags for some bindings
// spills are put into registers

// the main function here should get a usage map and
// try allocating cpu flags to bindings that can benefit of just being flags (no registers).
// bindings that contained flags that are spilled are returned back with the CPU flag they
// represent.

#[derive(Debug)]
enum CPUFlag {
}
