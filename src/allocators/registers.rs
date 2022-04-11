//! Register analysis of the code
use crate::codegen::assembly::RegisterID;
use crate::intermediate::{analysis::CollisionMap, Binding, BlockEnd, IR};
use std::collections::HashMap;
use std::collections::HashSet;

pub type RegisterMap = HashMap<Binding, RegisterID>;

#[derive(Debug, PartialEq, Eq)]
pub enum AllocatorHint {
    /// Returned from a function, which means that the binding will
    /// get an allocation for w0 immediately.
    UsedInReturn,
    /// The binding is declared by a phi node of other bindings.
    /// If all of the other bindings share the same allocation,
    /// the target binding is ensured to share it as well.
    UsedInPhiNode(HashSet<Binding>),
    /// There's at least one call while the binding is alive.
    LivesThroughCall,
    /// Already destined to be a memory allocation.
    AlreadyInMemory,
}

impl PartialOrd for AllocatorHint {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::LivesThroughCall, Self::LivesThroughCall) => Some(std::cmp::Ordering::Equal),
            (Self::LivesThroughCall, _) => Some(std::cmp::Ordering::Greater),
            (_, Self::LivesThroughCall) => Some(std::cmp::Ordering::Less),
            (Self::UsedInReturn, Self::UsedInPhiNode(_)) => Some(std::cmp::Ordering::Greater),
            (Self::UsedInReturn, _) => Some(std::cmp::Ordering::Equal),
            (Self::UsedInPhiNode(_), Self::UsedInPhiNode(_)) => Some(std::cmp::Ordering::Equal),
            _ => None,
        }
    }
}

impl Ord for AllocatorHint {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::LivesThroughCall, Self::LivesThroughCall) => std::cmp::Ordering::Equal,
            (Self::LivesThroughCall, _) => std::cmp::Ordering::Greater,
            (_, Self::LivesThroughCall) => std::cmp::Ordering::Less,
            (Self::UsedInReturn, Self::UsedInPhiNode(_)) => std::cmp::Ordering::Greater,
            (Self::UsedInReturn, _) => std::cmp::Ordering::Equal,
            (Self::UsedInPhiNode(_), Self::UsedInPhiNode(_)) => std::cmp::Ordering::Equal,
            _ => std::cmp::Ordering::Equal,
        }
    }
}

pub fn make_allocator_hints(code: &IR) -> HashMap<Binding, Vec<AllocatorHint>> {
    let mut map = HashMap::<Binding, Vec<AllocatorHint>>::new();
    for block in crate::intermediate::analysis::TopBottomTraversal::from(code) {
        for statement in &code[block].statements {
            if let crate::intermediate::Statement::Assign { index, value } = statement {
                match value {
                    crate::intermediate::Value::Phi { nodes } => {
                        let other_bindings: HashSet<_> = nodes
                            .into_iter()
                            .filter_map(|descriptor| descriptor.value.as_binding())
                            .collect();
                        map.entry(*index)
                            .or_default()
                            .push(AllocatorHint::UsedInPhiNode(other_bindings));
                    }
                    crate::intermediate::Value::Allocate { .. } => map
                        .entry(*index)
                        .or_default()
                        .push(AllocatorHint::AlreadyInMemory),
                    _ => (),
                }
                // TODO: match call
            }
        }
        if let BlockEnd::Return(binding) = code[block].end {
            map.entry(binding)
                .or_default()
                .push(AllocatorHint::UsedInReturn)
        }
    }
    map
}

#[allow(dead_code)] // this function will be used upon having calls, don't worry
fn is_callee_saved(register: u8) -> bool {
    (9..=15).contains(&register)
}

#[derive(Debug)]
pub struct CodegenHints {
    #[allow(dead_code)]
    /// list of bindings that couldn't be allocated the return register but are to be returned.
    pub need_move_to_return_reg: HashSet<Binding>,
    /// list of bindings that couldn't be allocated a callee-saved register so they need to be
    /// pre-saved before a call is met.
    #[allow(dead_code)]
    pub save_upon_call: HashSet<Binding>,

    /// set of bindings that could not be allocated, for whatever reason
    #[allow(dead_code)]
    pub completely_spilled: HashSet<Binding>,

    /// The register assigned for the bindings
    pub registers: RegisterMap,
}

pub struct AllocatorState {
    /// list of bindings that will presave their register during their lifetime
    save_during_usage_hints: HashSet<Binding>,
    /// list of bindings that couldn't be allocated the return register but are to be returned.
    need_move_to_return_reg: HashSet<Binding>,
    /// list of bindings that couldn't be allocated a callee-saved register so they need to be
    /// pre-saved before a call is met.
    save_when_call: HashSet<Binding>,
    /// buckets for allocations
    buckets: [HashSet<Binding>; 31],
}

impl std::fmt::Debug for AllocatorState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct DebugBuckets<'s>(&'s [HashSet<Binding>]);
        struct DebugBucket<'s>(usize, &'s HashSet<Binding>);
        impl std::fmt::Debug for DebugBucket<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "r{} => {:?}", self.0, self.1)
            }
        }

        impl std::fmt::Debug for DebugBuckets<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_list()
                    .entries(
                        self.0
                            .iter()
                            .enumerate()
                            .filter(|(_, s)| !s.is_empty())
                            .map(|(a, b)| DebugBucket(a, b)),
                    )
                    .finish()
            }
        }

        let mut dbg_st = f.debug_struct("AllocatorState");
        if !self.save_during_usage_hints.is_empty() {
            dbg_st.field("save for usage", &self.save_during_usage_hints);
        }
        if !self.need_move_to_return_reg.is_empty() {
            dbg_st.field("move back when returning", &self.need_move_to_return_reg);
        }
        if self.buckets.iter().any(|s| !s.is_empty()) {
            dbg_st.field("allocations", &DebugBuckets(&self.buckets));
        }
        dbg_st.finish()
    }
}

impl AllocatorState {
    pub fn new() -> Self {
        Self {
            save_during_usage_hints: HashSet::new(),
            need_move_to_return_reg: HashSet::new(),
            save_when_call: HashSet::new(),
            buckets: Default::default(),
        }
    }

    fn get_sorted_indices(
        &self,
        range: impl Iterator<Item = usize>,
    ) -> impl Iterator<Item = usize> {
        let mut all: Vec<_> = range.into_iter().collect();
        all.sort_by(|a, b| {
            self.buckets[*a]
                .len()
                .cmp(&self.buckets[*b].len())
                .reverse()
        });
        all.into_iter()
    }

    pub fn try_register(
        &mut self,
        binding: Binding,
        collides_with: &HashSet<Binding>,
        register: usize,
    ) -> bool {
        if self.buckets[register].is_disjoint(collides_with) {
            self.buckets[register].insert(binding);
            return true;
        } else {
            return false;
        }
    }

    pub fn try_nonsaved_register(
        &mut self,
        binding: Binding,
        collides_with: &HashSet<Binding>,
    ) -> bool {
        self.get_sorted_indices((0..9).chain(16..31))
            .any(|bucket| self.try_register(binding, collides_with, bucket))
    }

    pub fn try_saved_register(
        &mut self,
        binding: Binding,
        collides_with: &HashSet<Binding>,
    ) -> bool {
        self.get_sorted_indices(9..=15)
            .any(|bucket| self.try_register(binding, collides_with, bucket))
    }

    // try non-saved; then follow by saved
    pub fn try_standard_alloc(
        &mut self,
        binding: Binding,
        collides_with: &HashSet<Binding>,
    ) -> bool {
        self.try_nonsaved_register(binding, collides_with)
            || self.try_saved_register(binding, collides_with)
    }
}

pub fn alloc_registers(
    collisions: &CollisionMap,
    need_allocation: Vec<Binding>,
    alloc_hints: HashMap<Binding, Vec<AllocatorHint>>,
) -> CodegenHints {
    let mut state = AllocatorState::new();
    // strategy #1: For non-returned bindings, non-phi bindings, bindings that don't need to live after a call
    // we're just going to try and allocate
    // those to non-callee-saved registers.
    // returns the bindings that it could not allocate.
    let mut could_not_allocate = HashSet::new();
    let mut allocated = HashSet::new();
    'hints: for (binding, mut hints) in alloc_hints {
        hints.sort();
        let mut is_call = false;
        let mut is_return = false;
        let mut phi_nodes: Option<HashSet<Binding>> = None;
        let mut locked_phi_nodes = false;
        for hint in hints {
            match hint {
                AllocatorHint::UsedInReturn => is_return = true,
                AllocatorHint::UsedInPhiNode(others) => {
                    if !locked_phi_nodes {
                        phi_nodes = match phi_nodes {
                            Some(already_in) => {
                                // we can have more!
                                if already_in.is_superset(&others) || already_in.is_subset(&others)
                                {
                                    Some(already_in.union(&others).cloned().collect())
                                } else {
                                    // but if not, we're not going to accept more phi suggestions, just
                                    // allocate to another register and that's it.
                                    locked_phi_nodes = true;
                                    None
                                }
                            }
                            // we haven't had any other phi suggestions.
                            None => Some(others),
                        }
                    }
                }
                AllocatorHint::LivesThroughCall => is_call = true,
                // Spill already since it's set to be in memory.
                AllocatorHint::AlreadyInMemory => {
                    could_not_allocate.insert(binding);
                    continue 'hints;
                }
            }
        }

        let collisions = collisions.get(&binding).unwrap();
        allocated.insert(binding);
        if is_call {
            // if the binding is returned, we *probably* assigned a non-return register, so set it
            // to move to return
            if is_return {
                state.need_move_to_return_reg.insert(binding);
            }
            // we're going to look through the buckets and check our collisions to find if we can
            // allocate a callee-saved register
            if state.try_saved_register(binding, collisions) {
                continue;
            }
            // could not find an empty bucket from the callee-saved ones, so we'll try an empty of
            // the other ones:
            else if state.try_nonsaved_register(binding, collisions) {
                state.save_when_call.insert(binding);
                continue;
            } else {
                // otherwise we'll have to use memory.
                could_not_allocate.insert(binding);
            }
        } else if is_return {
            // try to allocate the return register.
            if !state.try_register(binding, collisions, 0) {
                state.need_move_to_return_reg.insert(binding);
                // try to allocate in a non-callee-saved register.

                if !state.try_standard_alloc(binding, collisions) {
                    // otherwise we'll have to use memory.
                    could_not_allocate.insert(binding);
                }
            }
        }
    }

    // now we'll walk the rest of the bindings without hints and try a standard strategy: Allocate
    // non-callee-saved and then callee-saved or nothing.
    'bindings: for binding in need_allocation
        .into_iter()
        .filter(|x| !allocated.contains(x))
    {
        let collisions = collisions.get(&binding).unwrap();
        if !state.try_standard_alloc(binding, collisions) {
            could_not_allocate.insert(binding);
        }
    }

    debug_assert!(
        could_not_allocate.is_empty(),
        "spills aren't yet implemented"
    );
    debug_assert!(
        state.save_when_call.is_empty(),
        "save when call spill kinds are not yet implemented"
    );
    CodegenHints {
        need_move_to_return_reg: state.need_move_to_return_reg,
        save_upon_call: state.save_when_call,
        completely_spilled: could_not_allocate,
        registers: state
            .buckets
            .into_iter()
            .enumerate()
            .flat_map(|(register, bindings)| {
                bindings
                    .into_iter()
                    .map(move |binding| (binding, RegisterID(register as u8)))
            })
            .collect(),
    }
}

// TODO: collides != contains. We need `contains` for making scopes.

//fn find_bindings_from_condition(ir: &IR) -> HashMap<Binding, Condition> {}

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

#[cfg(test)]
mod tests {

    // This function does not test anything. Only serves as a mediant to get through the previous
    // parsing & compilation steps that already work correctly.
    // Any analysis performed on the IR will be stated explicitly, even if it could be factored
    // out.
    fn compile_source_into_ir(source: &str) -> anyhow::Result<crate::intermediate::IR> {
        let meta = crate::error::SourceMetadata::new(source).with_file("<test program>".into());
        let program = crate::grammar::Parser::new(&meta).parse()?;
        let (_function_name, ir) = crate::intermediate::generate::compile_program(program, &meta)?;
        Ok(ir)
    }

    #[test]
    fn should_all_allocate() {
        let ir = compile_source_into_ir(
            r#"
int main() {
  return 5 > 1 + 2;
}"#,
        )
        .unwrap();
        let collisions = crate::intermediate::analysis::compute_lifetime_collisions(&ir);
        // TODO: test traversal for allocator hints
        let hints = crate::codegen::registers::make_allocator_hints(&ir);
        let result = crate::codegen::registers::alloc_registers(
            &collisions,
            collisions.keys().cloned().collect(),
            hints,
        );
        assert!(
            result.need_move_to_return_reg.is_empty(),
            "The return register should be directly available for the needing binding"
        );
        assert!(result.save_upon_call.is_empty(), "Program does no calls");
        assert!(
            result.completely_spilled.is_empty(),
            "There should be no left outs for this program"
        );
    }
}