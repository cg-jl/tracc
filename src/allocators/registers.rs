//! Register analysis of the code
use crate::asmgen::assembly::{Condition, RegisterID};
use crate::ir::{analysis::CollisionMap, Binding, BlockEnd, IR};
use std::collections::HashMap;
use std::collections::HashSet;

// TODO: when failing to produce same register on phi nodes, make sure after allocations
// that those registers that go to phi nodes are put correctly before each branch.

pub type RegisterMap = HashMap<Binding, RegisterID>;

#[derive(Debug)]
pub enum AllocatorHints {
    InMemory,
    Usable {
        used_in_return: bool,
        is_zero: bool,
        used_through_call: bool,
        from_phi_node: Option<HashSet<Binding>>,
        is_phi_node_with: HashSet<Binding>,
    },
}

#[derive(Debug)]
enum ImmediateHint {
    InMemory,
    Usable {
        used_in_return: bool,
        is_zero: bool,
        used_through_call: bool,
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
                is_phi_node_with: HashSet::new(),
                used_through_call: false,
                used_in_return: false,
                is_zero: false,
                from_phi_node: None,
            },
        }
    }

    pub fn add_phi_edge(&mut self, bindings: HashSet<Binding>) {
        if let AllocatorHints::Usable {
            is_phi_node_with, ..
        } = &mut self.hints
        {
            is_phi_node_with.extend(bindings);
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
    for block in crate::ir::analysis::TopBottomTraversal::from(code) {
        for statement in &code[block].statements {
            if let crate::ir::Statement::Assign { index, value } = statement {
                match value {
                    crate::ir::Value::Constant(0) => {
                        map.entry(*index).or_default().caught_zero();
                    }
                    crate::ir::Value::Phi { nodes } => {
                        let other_bindings: HashSet<_> =
                            nodes.iter().map(|descriptor| descriptor.value).collect();
                        nodes
                            .iter()
                            .map(|descriptor| descriptor.value)
                            .for_each(|node| {
                                let mut all = other_bindings.clone();
                                all.remove(&node); // remove self
                                map.entry(node).or_default().add_phi_edge(all);
                            });
                        map.entry(*index).or_default().add_phi_node(other_bindings);
                    }
                    crate::ir::Value::Allocate { .. } => {
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

#[derive(Debug, Default)]
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

    /// Set of bindings that are just to store a condition.
    pub stores_condition: HashMap<Binding, Condition>,

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
        tracing::trace!(target: "register_alloc::state", "Collisions: {collides_with:?} \\ {bucket:?}",
                        bucket=&self.buckets[register as usize]);
        if self.buckets[register as usize].is_disjoint(collides_with) {
            tracing::trace!(target: "register_alloc::state", "Found available {binding} -> x{register}");
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

// To simplify allocation, we're going to split them by blocks. Each block gets its starts and ends
// for each binding being used.

#[derive(Debug)]
pub struct ActiveBindingSet {
    pub bindings: Vec<Binding>,
}

use crate::ir::analysis::lifetimes::BlockAddress;

impl ActiveBindingSet {
    pub fn add(&mut self, binding: Binding, ends: &HashMap<Binding, usize>) {
        let binding_end = ends[&binding];
        for i in 0..self.bindings.len() {
            if ends[&self.bindings[i]] > binding_end {
                self.bindings.insert(i, binding);
                return;
            }
        }
        self.bindings.push(binding);
    }

    pub fn last(&self) -> Option<Binding> {
        self.bindings.last().copied()
    }

    pub fn contains(&self, binding: Binding) -> bool {
        self.bindings.contains(&binding)
    }

    pub fn remove(&mut self, binding: Binding) -> bool {
        let found = self
            .bindings
            .iter()
            .copied()
            .position(|other| other == binding);

        if let Some(i) = found {
            self.bindings.remove(i);
            true
        } else {
            false
        }
    }

    pub const fn new() -> Self {
        Self {
            bindings: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.bindings.len()
    }
}

type BindingGraph = HashMap<Binding, HashSet<Binding>>;

// code stolen from sawblade, with some touches for phi nodes
// NOTE: considers everything is 'dead' at the end of the block.
fn linear_alloc_block(
    codegen_hints: &mut CodegenHints,
    phi_nodes: &mut BindingGraph,
    phi_edges: &mut BindingGraph,
    ordered_bindings_by_start: &[Binding],
    used_through_call: &HashSet<Binding>,
    used_in_return: &HashSet<Binding>,
    starts: &HashMap<Binding, usize>,
    ends: &HashMap<Binding, usize>,
) {
    let mut active = ActiveBindingSet::new();
    let mut used = HashMap::new();

    tracing::trace!(target: "alloc::registers::block", "ordered by start: {ordered_bindings_by_start:?}");
    tracing::trace!(target: "alloc::registers::block", "ends: {ends:?}");
    tracing::trace!(target: "alloc::registers::block", "starts: {starts:?}");

    for binding in ordered_bindings_by_start.iter().copied() {
        let start = starts[&binding];
        tracing::trace!(target: "alloc::registers::block", "allocating {binding}, which starts at {start}");
        // expire old intervals
        let end_i = active
            .bindings
            .iter()
            .position(|other| ends[other] > start)
            .unwrap_or(active.bindings.len());

        for dropped_binding in active.bindings.drain(..end_i) {
            tracing::trace!(target: "alloc::registers", "dropping {dropped_binding}");
            let reg = &codegen_hints.registers[&dropped_binding];
            if used[reg] == 1 {
                used.remove(reg);
            } else {
                // SAFE: the if above already asserted that we have the value.
                *unsafe { used.get_mut(reg).unwrap_unchecked() } -= 1;
            }
        }

        // do not allocate it. This way, if there's something wrong, codegen will panic saying
        // that there was a binding that wasn't allocated but is needed inside a register.
        if codegen_hints.stores_condition.contains_key(&binding) {
            tracing::trace!(target: "alloc::registers", "{binding} was moved to store a flag");
            continue;
        }

        // sometimes the binding has been already allocated and we're just extending
        // its lifetime, so it's ok
        if let Some(already_allocated) = codegen_hints.registers.get(&binding).copied() {
            tracing::trace!(target: "alloc::registers", "{binding} already allocated to {already_allocated:?}");
            *used.entry(already_allocated).or_insert(0) += 1;
            active.add(binding, ends);
            continue;
        }

        let phi_alloc =  phi_edges
            .remove(&binding)
            .and_then(|edges| {
                let allocs: HashSet<_> = edges
                    .iter()
                    .flat_map(|binding| codegen_hints.registers.get(binding).copied())
                    .collect();

                if allocs.is_empty() {
                    None // not going to do anything, use another hint
                } else {
                    tracing::debug!(target: "register_alloc::phi::full", "found phi edges: {edges:?}");
                    // allocate the same as the rest of bindings that it is target to
                    debug_assert_eq!(
                        allocs.len(),
                        1,
                        "Expecting all allocated edges to be in the same place"
                        );
                    // SAFE: assertion above.
                    let unique_alloc = unsafe { allocs.into_iter().next().unwrap_unchecked() };
                    tracing::trace!(target: "register_alloc::phi::full", "setting allocation through phi nodes: {unique_alloc:?}");

                    Some(unique_alloc)
                }
            })
        .or_else(|| {
            phi_nodes.remove(&binding).map(|phi_nodes| {
                // NOTE: might have to reorder allocations so that collisions are resolved
                // must have allocated everyone
                let allocs: Vec<_> = phi_nodes
                    .iter()
                    .flat_map(|binding| codegen_hints.registers.get(binding).copied())
                    .collect();

                tracing::trace!(target: "alloc::registers", "found phi nodes: {phi_nodes:?} | {allocs:?}");
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
                tracing::trace!(target: "alloc::registers", "found allocated phi node: {unique_alloc:?}");

                unique_alloc
            })
        });

        // try to find an available register.
        let unused_register = phi_alloc.or_else(|| {
            if used_through_call.contains(&binding) {
                // try a callee-saved register, otherwise hint that we couldn't
                (9..=15)
                    .map(RegisterID::from)
                    .find(|reg| !used.contains_key(reg))
                    .or_else(|| {
                        (0..15)
                            .chain((16..=30))
                            .map(RegisterID::from)
                            .find(|reg| !used.contains_key(reg))
                            .map(|reg| {
                                codegen_hints.save_upon_call.insert(binding);
                                reg
                            })
                    })
            } else {
                (0..15)
                    .chain((16..=30))
                    .chain((9..=15))
                    .map(RegisterID::from)
                    .find(|reg| !used.contains_key(reg))
            }
        });

        // spill one to get this one
        let found_register = unused_register.or_else(|| {
            // SAFE: we know that there must be at least one active binding, since all of our
            // registers are occupied.
            let longest_lived = unsafe { active.last().unwrap_unchecked() };
            if ends[&longest_lived] > ends[&binding] {

                tracing::trace!(target: "register_alloc", "spilling {longest_lived} in favor of {binding}");
                // spill the longest lived and allocate this one since it has a shorter span.
                active.remove(longest_lived);
                codegen_hints.completely_spilled.insert(longest_lived);
                let reg = unsafe {
                    codegen_hints
                        .registers
                        .insert(longest_lived, RegisterID::StackPointer)
                        .unwrap_unchecked()
                };
                *used.entry(reg).or_default() += 1;
                Some(reg)
            } else {
                None
            }
        });

        if let Some(register) = found_register {
            tracing::trace!(target: "alloc::registers", "found register {register:?} for {binding}");
            assert!(
                used.insert(register, 1).is_none(),
                "this register shouldn't be being used!"
            );
            codegen_hints.registers.insert(binding, register);
            active.add(binding, ends);
        } else {
            codegen_hints.completely_spilled.insert(binding);
            codegen_hints
                .registers
                .insert(binding, RegisterID::StackPointer);
        }
    }
}

pub fn alloc_registers(
    ir: &IR,
    need_allocation: Vec<Binding>,
    alloc_hints: HashMap<Binding, AllocatorHints>,
) -> CodegenHints {
    tracing::trace!(target: "register_alloc", "requested allocations: {need_allocation:?}");
    tracing::trace!(target: "register_alloc", "allocator hints: {alloc_hints:?}");
    // TODO: reserve phi nodes hints for later, when all of its dependencies are allocated.
    // and so on.
    let mut state = AllocatorState::new();
    let mut might_need_call_save = HashSet::new();
    let mut might_need_move_to_x0 = HashSet::new();
    let mut codegen_hints = CodegenHints::default();
    let (immediate_alloc_hints, mut phi_nodes, mut phi_edges, used_in_return, used_through_call) = {
        let mut used_in_return_set = HashSet::new();
        let mut used_through_call_set = HashSet::new();
        let mut immediate_allocs = HashMap::new();
        let mut phi_nodes = HashMap::new();
        let mut phi_targets = HashMap::new();

        for (binding, hints) in alloc_hints {
            match hints {
                AllocatorHints::InMemory => {
                    tracing::debug!(target: "register_alloc::hints", "found memory use case: {binding}");
                    immediate_allocs.insert(binding, ImmediateHint::InMemory);
                }
                AllocatorHints::Usable {
                    used_in_return,
                    is_zero,
                    used_through_call,
                    from_phi_node,
                    is_phi_node_with,
                } => {
                    if used_through_call {
                        used_through_call_set.insert(binding);
                        tracing::debug!(target: "register_alloc::hints", "found used through call: {binding}");
                        might_need_call_save.insert(binding);
                    }
                    if used_in_return {
                        used_in_return_set.insert(binding);

                        tracing::debug!(target: "register_alloc::hints", "found used in return: {binding}");
                        might_need_move_to_x0.insert(binding);
                    }

                    // only put it in zero register
                    if is_zero && !used_in_return {
                        tracing::debug!(target: "register_alloc::hints", "{binding} is zero");
                        codegen_hints
                            .registers
                            .insert(binding, RegisterID::ZeroRegister);
                    }

                    immediate_allocs.insert(
                        binding,
                        ImmediateHint::Usable {
                            used_in_return,
                            is_zero,
                            used_through_call,
                        },
                    );
                    if let Some(nodes) = from_phi_node {
                        tracing::trace!(target: "register_alloc::hints", "found source phi node for {binding}: {nodes:?}");
                        phi_nodes.insert(binding, nodes);
                    }
                    if !is_phi_node_with.is_empty() {
                        tracing::trace!(target: "register_alloc::hints", "found phi node usage neighbors for {binding}: {is_phi_node_with:?}");
                        phi_targets.insert(binding, is_phi_node_with);
                    }
                }
            };
        }

        (
            immediate_allocs,
            phi_nodes,
            phi_targets,
            used_in_return_set,
            used_through_call_set,
        )
    };

    codegen_hints.stores_condition = super::flag::get_used_flags(ir).collect();

    use crate::ir::analysis;

    let mut all_lifetimes = analysis::lifetimes::make_sorted_lifetimes(ir);

    for full_lifetime in all_lifetimes {
        linear_alloc_block(
            &mut codegen_hints,
            &mut phi_nodes,
            &mut phi_edges,
            &full_lifetime.ordered_by_start,
            &used_through_call,
            &used_in_return,
            &full_lifetime.binding_starts,
            &full_lifetime.binding_ends,
        );
    }

    codegen_hints
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
    fn compile_source_into_ir(source: &str) -> anyhow::Result<crate::ir::IR> {
        let meta = crate::error::SourceMetadata::new(source).with_file("<test program>".into());
        let program = crate::grammar::Parser::new(&meta).parse()?;
        let (_function_name, ir) = crate::ir::generate::compile_function(program, &meta)?;
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
        let lifetimes = crate::ir::analysis::compute_lifetimes(&ir);
        let collisions =
            crate::ir::analysis::compute_lifetime_collisions(&ir, &lifetimes);
        // TODO: test traversal for allocator hints
        let hints = make_allocator_hints(&ir);
        let result = alloc_registers(&ir, collisions.keys().cloned().collect(), hints);
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
