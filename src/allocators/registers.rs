//! Register analysis of the code
use crate::codegen::assembly::RegisterID;
use crate::intermediate::{analysis::CollisionMap, Binding, BlockEnd, IR};
use std::collections::HashMap;
use std::collections::HashSet;

// TODO: when failing to produce same register on phi nodes, make sure after allocations
// that those registers that go to phi nodes are put correctly before each branch.

pub type RegisterMap = HashMap<Binding, RegisterID>;

pub enum AllocatorHints {
    InMemory,
    Usable {
        used_in_return: bool,
        is_zero: bool,
        used_through_call: bool,
        from_phi_node: Option<HashSet<Binding>>,
    },
}

struct HintBuilder {
    phi_nodes_locked: bool,
    hints: AllocatorHints,
}

impl HintBuilder {
    pub fn new() -> Self {
        Self {
            phi_nodes_locked: false,
            hints: AllocatorHints::Usable {
                used_through_call: false,
                used_in_return: false,
                is_zero: false,
                from_phi_node: None,
            },
        }
    }

    pub fn caught_used_through_call(&mut self) {
        if let AllocatorHints::Usable {
            used_through_call, ..
        } = &mut self.hints
        {
            *used_through_call = true;
        }
    }

    pub fn caught_returned(&mut self) {
        if let AllocatorHints::Usable { used_in_return, .. } = &mut self.hints {
            *used_in_return = true;
        }
    }

    pub fn caught_zero(&mut self) {
        if let AllocatorHints::Usable { is_zero, .. } = &mut self.hints {
            *is_zero = true;
        }
    }

    pub fn add_phi_node(&mut self, others: HashSet<Binding>) {
        if !self.phi_nodes_locked {
            if let AllocatorHints::Usable { from_phi_node, .. } = &mut self.hints {
                match from_phi_node {
                    Some(ref mut already_in) => {
                        // we can have more
                        if already_in.is_subset(&others) {
                            already_in.extend(others);
                        } else if !already_in.is_superset(&others) {
                            // since we've got discrepancies, phi nodes will be locked
                            // to `None`, since I don't know what to do here.
                            self.phi_nodes_locked = true;
                            *from_phi_node = None;
                        }
                    }
                    None => *from_phi_node = Some(others),
                }
            }
        }
    }

    pub fn finish(self) -> AllocatorHints {
        self.hints
    }

    pub fn value_is_memory(&mut self) {
        self.hints = AllocatorHints::InMemory;
    }
}

impl Default for HintBuilder {
    fn default() -> Self {
        Self::new()
    }
}

pub fn make_allocator_hints(code: &IR) -> HashMap<Binding, AllocatorHints> {
    let mut map = HashMap::<Binding, HintBuilder>::new();
    for block in crate::intermediate::analysis::TopBottomTraversal::from(code) {
        for statement in &code[block].statements {
            if let crate::intermediate::Statement::Assign { index, value } = statement {
                match value {
                    crate::intermediate::Value::Constant(0) => {
                        map.entry(*index).or_default().caught_zero();
                    }
                    crate::intermediate::Value::Phi { nodes } => {
                        let other_bindings: HashSet<_> =
                            nodes.iter().map(|descriptor| descriptor.value).collect();
                        map.entry(*index).or_default().add_phi_node(other_bindings);
                    }
                    crate::intermediate::Value::Allocate { .. } => {
                        map.entry(*index).or_default().value_is_memory()
                    }
                    _ => (),
                }
                // TODO: match call
            }
        }
        if let BlockEnd::Return(binding) = code[block].end {
            map.entry(binding).or_default().caught_returned();
        }
    }
    map.into_iter()
        .map(|(binding, builder)| (binding, builder.finish()))
        .collect()
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
    /// list of bindings that couldn't be allocated a register.
    spills: HashSet<Binding>,
    /// list of bindings that got allocated the zero register.
    got_zero: HashSet<Binding>,
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

impl Default for AllocatorState {
    fn default() -> Self {
        Self::new()
    }
}

impl AllocatorState {
    pub fn new() -> Self {
        Self {
            save_during_usage_hints: HashSet::new(),
            need_move_to_return_reg: HashSet::new(),
            save_when_call: HashSet::new(),
            spills: HashSet::new(),
            got_zero: HashSet::new(),
            buckets: Default::default(),
        }
    }

    fn get_sorted_indices(&self, range: impl Iterator<Item = u8>) -> impl Iterator<Item = u8> {
        let mut all: Vec<_> = range.into_iter().collect();
        all.sort_by(|a, b| {
            self.buckets[*a as usize]
                .len()
                .cmp(&self.buckets[*b as usize].len())
                .reverse()
        });
        all.into_iter()
    }

    pub fn is_zero(&mut self, binding: Binding) -> RegisterID {
        self.got_zero.insert(binding);
        RegisterID::ZeroRegister
    }

    pub fn try_alloc(
        &mut self,
        binding: Binding,
        collides_with: &HashSet<Binding>,
        alloc: RegisterID,
    ) -> Option<RegisterID> {
        match alloc {
            RegisterID::ZeroRegister => Some(self.is_zero(binding)),
            RegisterID::GeneralPurpose { index } => {
                self.try_register(binding, collides_with, index)
            }
            RegisterID::StackPointer => Some(self.spill(binding)),
        }
    }

    pub fn try_register(
        &mut self,
        binding: Binding,
        collides_with: &HashSet<Binding>,
        register: u8,
    ) -> Option<RegisterID> {
        if self.buckets[register as usize].is_disjoint(collides_with) {
            self.buckets[register as usize].insert(binding);
            Some(RegisterID::GeneralPurpose { index: register })
        } else {
            None
        }
    }

    pub fn try_nonsaved_register(
        &mut self,
        binding: Binding,
        collides_with: &HashSet<Binding>,
    ) -> Option<RegisterID> {
        self.get_sorted_indices((0..9).chain(16..31))
            .find_map(|bucket| self.try_register(binding, collides_with, bucket as u8))
    }

    pub fn try_saved_register(
        &mut self,
        binding: Binding,
        collides_with: &HashSet<Binding>,
    ) -> Option<RegisterID> {
        self.get_sorted_indices(9..=15)
            .find_map(|bucket| self.try_register(binding, collides_with, bucket as u8))
    }

    // try non-saved; then follow by saved
    pub fn try_standard_alloc(
        &mut self,
        binding: Binding,
        collides_with: &HashSet<Binding>,
    ) -> Option<RegisterID> {
        self.try_nonsaved_register(binding, collides_with)
            .or_else(|| self.try_saved_register(binding, collides_with))
    }

    pub fn spill(&mut self, binding: Binding) -> RegisterID {
        self.spills.insert(binding);
        RegisterID::StackPointer
    }

    pub fn get_allocation(&self, binding: Binding) -> Option<RegisterID> {
        self.spills
            .get(&binding)
            .map(|_| RegisterID::StackPointer)
            .or_else(|| {
                self.buckets.iter().enumerate().find_map(|(index, set)| {
                    if set.contains(&binding) {
                        Some(RegisterID::GeneralPurpose { index: index as u8 })
                    } else {
                        None
                    }
                })
            })
    }
}

pub fn alloc_registers(
    ir: &IR,
    collisions: &CollisionMap,
    need_allocation: Vec<Binding>,
    mut alloc_hints: HashMap<Binding, AllocatorHints>,
) -> CodegenHints {
    // TODO: reserve phi nodes hints for later, when all of its dependencies are allocated.
    // and so on.
    let mut state = AllocatorState::new();
    // strategy #1: For non-returned bindings, non-phi bindings, bindings that don't need to live after a call
    // we're just going to try and allocate
    // those to non-callee-saved registers.
    // returns the bindings that it could not allocate.
    let mut allocated = HashSet::new();
    'hints: for binding in need_allocation {
        if allocated.contains(&binding) {
            continue;
        }
        allocated.insert(binding);
        let collisions = collisions.get(&binding).unwrap();
        let hinted_alloc = match alloc_hints.remove(&binding) {
            Some(hints) => match hints {
                AllocatorHints::Usable {
                    used_in_return,
                    is_zero,
                    used_through_call,
                    from_phi_node,
                } => {
                    if used_through_call {
                        // if the binding is returned, we *probably* assigned a non-return register, so set it
                        // to move to return
                        if used_in_return {
                            state.need_move_to_return_reg.insert(binding);
                        }
                        // we're going to look through the buckets and check our collisions to find if we can
                        // allocate a callee-saved register
                        state.try_saved_register(binding, collisions).or_else(|| {
                            state.try_nonsaved_register(binding, collisions).map(|x| {
                                state.save_when_call.insert(binding);
                                x
                            })
                        })
                    } else if let Some(phi_nodes) = from_phi_node {
                        // NOTE: might have to reorder allocations so that collisions are resolved
                        // must have allocated everyone
                        let allocs: Vec<_> = phi_nodes
                            .iter()
                            .flat_map(|binding| state.get_allocation(*binding))
                            .collect();

                        debug_assert_eq!(
                            allocs.len(),
                            phi_nodes.len(),
                            "All phi nodes must have been previously allocated"
                        );

                        let allocs: HashSet<_> = allocs.into_iter().collect();

                        // NOTE: this will be removed if needed
                        debug_assert_eq!(
                            allocs.len(),
                            1,
                            "All allocated phi nodes must be on the same register"
                        );

                        let unique_alloc = allocs.into_iter().next().unwrap();

                        debug_assert!(
                            state.try_alloc(binding, collisions, unique_alloc).is_some(),
                            "Must be able to allocate binding on same register as its phi nodes"
                        );
                        Some(unique_alloc)
                    } else if used_in_return {
                        state.try_register(binding, collisions, 0).or_else(|| {
                            state.try_standard_alloc(binding, collisions).map(|x| {
                                state.need_move_to_return_reg.insert(binding);
                                x
                            })
                        })
                    } else {
                        is_zero.then(|| state.is_zero(binding))
                    }
                }
                AllocatorHints::InMemory => {
                    state.spill(binding);
                    continue 'hints;
                }
            },
            None => None,
        };

        let final_alloc = hinted_alloc.or_else(|| state.try_standard_alloc(binding, collisions));

        if final_alloc.is_none() {
            state.spill(binding);
        }
    }

    CodegenHints {
        need_move_to_return_reg: state.need_move_to_return_reg,
        save_upon_call: state.save_when_call,
        completely_spilled: state.spills,
        registers: state
            .buckets
            .into_iter()
            .enumerate()
            .flat_map(|(register, bindings)| {
                bindings.into_iter().map(move |binding| {
                    (
                        binding,
                        RegisterID::GeneralPurpose {
                            index: register as u8,
                        },
                    )
                })
            })
            .chain(
                state
                    .got_zero
                    .into_iter()
                    .map(|binding| (binding, RegisterID::ZeroRegister)),
            )
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
    use super::*;

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
        let hints = make_allocator_hints(&ir);
        let result = alloc_registers(
            &ir,
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
