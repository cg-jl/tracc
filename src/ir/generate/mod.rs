use std::collections::HashMap;
use std::fmt;
use std::mem::MaybeUninit;

use super::{
    BasicBlock, Binding, BlockBinding, BranchingMap, ByteSize, Condition, IRCode, Statement, Value,
    IR,
};
use crate::error::SourceMetadata;
use crate::grammar::lexer::Source;
use crate::ir::{BlockEnd, Branch, PhiDescriptor};
use crate::{ast, error};
mod block;
mod expr;
mod statement;
use thiserror::Error;

pub fn generate_branching_graphs(ir: &IRCode) -> (BranchingMap, BranchingMap) {
    let mut backwards_map = BranchingMap::new();
    let mut forward_map = BranchingMap::new();

    // go through the branches and add a `from` to the targets
    for (block_index, block) in ir.iter().enumerate() {
        let bb = BlockBinding(block_index);
        for branch in block.end.branch_list() {
            backwards_map.entry(branch).or_default().push(bb);
            forward_map.entry(bb).or_default().push(branch);
        }
    }

    (forward_map, backwards_map)
}

pub fn compile_program<'code>(
    ast::Program(functions): ast::Program<'code>,
    source_meta: &SourceMetadata<'code>,
) -> Result<(IR, Vec<&'code str>), Vec<StE>> {
    let mut state = IRGenState::default();
    let mut bindings = BindingCounter::default();
    let mut registered_functions = HashMap::with_capacity(functions.len());

    let mut errors = Vec::new();
    let mut names = Vec::with_capacity(functions.len());
    let mut function_entrypoints = Vec::with_capacity(functions.len());
    let mut function_argument_bindings = Vec::with_capacity(functions.len());

    for function in functions {
        let arg_count = function.args.len();
        let res = compile_function(
            &mut state,
            &mut bindings,
            &mut registered_functions,
            function,
            source_meta,
        );

        match res {
            Ok((name, range, arg_range)) => {
                names.push(name);
                function_entrypoints.push(range);
                function_argument_bindings.push(arg_range);
            }
            Err(e) => {
                errors.push(e);
            }
        }
    }

    if errors.is_empty() {
        let ir: IRCode = state.release().collect();
        let (forward_map, backwards_map) = generate_branching_graphs(&ir);

        let mut ir = IR {
            code: ir,
            backwards_map,
            forward_map,
            function_entrypoints,
            function_argument_bindings,
            function_block_ranges: Vec::new(), // NOTE: the function endpoints are filled only
                                               // in asmgen since we don't need them before.
        };

        tracing::info!(target: "irgen", "cleaning up generated code to remove garbo");
        tracing::debug!(target: "irgen", "ir before cleanup: {ir:?}");
        // run some cleanup on the generated code, because we might have generated
        // too much garbage
        super::cleanup::run_safe_cleanup(&mut ir);
        super::cleanup::prune_unreached_blocks(&mut ir);

        Ok((ir, names))
    } else {
        Err(errors)
    }
}

pub fn collect_phis(mut phis: impl Iterator<Item = PhiDescriptor>) -> Value {
    match phis.next() {
        None => Value::Uninit,
        Some(first) => match phis.next() {
            None => Value::Binding(first.value),
            Some(second) => {
                let mut nodes = vec![first, second];
                nodes.extend(phis);
                Value::Phi { nodes }
            }
        },
    }
}

fn compile_function<'code>(
    state: &mut IRGenState,
    bindings: &mut BindingCounter,
    functions: &mut HashMap<&'code str, BlockBinding>,
    f: ast::Function<'code>,
    source_meta: &SourceMetadata<'code>,
) -> Result<(&'code str, BlockBinding, core::ops::Range<usize>), StE> {
    let arg_range = bindings.latest_binding..bindings.latest_binding + f.args.len();
    tracing::trace!(target: "irgen::astshow", "compiling function: {f:?}");
    let ast::Function {
        name: ast::Identifier(name),
        args,
        body: ast::Block { statements },
    } = f;
    let mut env = VariableTracker::default();
    let mut entry = state.new_block();
    let function_entry = entry.block();

    // register the function. TODO: error when the function is stated twice.
    functions.insert(name, entry.block());

    // register all the arguments to be variables.
    // TODO: we'll have to make something in codegen to put the variables in the correct place,
    // both when calling and when receiving!
    {
        let env = env.variables_at_depth(0);
        for (variable, var_span) in args {
            let var_binding = bindings.next_binding();
            entry.allocate(var_binding, 4); // all the bindings are ints right now.
            env.insert(variable.0, (var_binding, ByteSize::U32));
        }
    }

    tracing::trace!(target: "irgen::function", "after putting variables in env: {env:?}");

    let mut end = block::compile_block(
        state,
        entry,
        statements,
        bindings,
        &mut env,
        functions,
        None,
        0,
        source_meta,
    )?;
    let ret = bindings.next_binding();
    end.assign(ret, 0);
    let end_index = end.block().0;
    end.finish_block(state, ret);

    Ok((name, function_entry, arg_range))
}

// TODO: make block builder struct
// TODO: make generators accept the current (unfinished) block as BlockBinding and return a
// BlockBinding (same block if not

pub struct BlockBuilder {
    current_block: Vec<Statement>,
    block_id: usize,
}

impl fmt::Debug for BlockBuilder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct DoDisplay<'a>(&'a Statement);

        impl<'a> fmt::Debug for DoDisplay<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }
        write!(f, "BlockBuilder(BB{}) ", self.block_id)?;
        f.debug_set()
            .entries(self.current_block.iter().map(DoDisplay))
            .finish()
    }
}

impl BlockBuilder {
    const fn new(block_id: usize) -> Self {
        Self {
            current_block: Vec::new(),
            block_id,
        }
    }
    pub fn assign(&mut self, binding: Binding, value: impl Into<Value>) {
        self.push(Statement::Assign {
            index: binding,
            value: value.into(),
        })
    }
    // TODO: use type ids and allocate by alignment, not by size (values have to be aligned)
    pub fn allocate(&mut self, target: Binding, size: usize) {
        self.assign(target, Value::Allocate { size })
    }
    pub fn load(&mut self, target: Binding, from_mem: Binding, size: ByteSize) {
        self.assign(
            target,
            Value::Load {
                mem_binding: from_mem,
                byte_size: size,
            },
        )
    }
    pub fn store(&mut self, from: Binding, mem: Binding, size: ByteSize) {
        self.push(Statement::Store {
            mem_binding: mem,
            binding: from,
            byte_size: size,
        })
    }
    pub fn push(&mut self, item: impl Into<Statement>) {
        self.current_block.push(item.into())
    }
    pub fn block(&self) -> BlockBinding {
        BlockBinding(self.block_id)
    }
    pub fn finish_block(
        self,
        state: &mut IRGenState,
        block_end: impl Into<BlockEnd>,
    ) -> BlockBinding {
        state.push_block(
            BasicBlock {
                statements: self.current_block,
                end: block_end.into(),
            },
            self.block_id,
        )
    }
}

// NOTE: uses `MaybeUninit` to preserve the relationship between indices and bindings
#[derive(Default)]
pub struct IRGenState {
    blocks: Vec<MaybeUninit<BasicBlock>>,
    given_builders: usize,
}

#[repr(transparent)]
#[derive(Default)]
pub struct BindingCounter {
    pub latest_binding: usize,
}

impl BindingCounter {
    pub fn next_binding(&mut self) -> Binding {
        let current = self.latest_binding;
        self.latest_binding += 1;
        Binding(current)
    }
}

impl IRGenState {
    // a binding is created once the block has finished
    fn push_block(&mut self, block: BasicBlock, block_id: usize) -> BlockBinding {
        // note: joining blocks so that they have always an end mark is done after everything has
        // been generated
        self.blocks[block_id].write(block);
        self.given_builders -= 1;
        BlockBinding(block_id)
    }
    // get a new block and a reference to it
    fn new_block(&mut self) -> BlockBuilder {
        let index = self.blocks.len();
        self.blocks.push(MaybeUninit::uninit());
        self.given_builders += 1;
        BlockBuilder::new(index)
    }

    fn release(self) -> impl Iterator<Item = BasicBlock> {
        debug_assert_eq!(
            self.given_builders, 0,
            "Thrown builders away without initialising their respective block"
        );
        self.blocks
            .into_iter()
            // UNSAFE: at this point I know all the blocks have been inserted correctly
            .map(|bb| unsafe { bb.assume_init() })
    }
}

type VariableMemories<'code> = HashMap<&'code str, (Binding, ByteSize)>;

#[derive(Debug)]
pub struct VariableTracker<'code> {
    memories: Vec<VariableMemories<'code>>,
}

impl<'code> VariableTracker<'code> {
    pub fn new() -> Self {
        Self {
            memories: Vec::new(),
        }
    }
    pub fn get(&self, name: &str, depth: usize) -> Option<&(Binding, ByteSize)> {
        tracing::trace!(target: "irgen::variables::get", "getting {name:?} from {depth}");
        self.memories[..=depth.min(self.memories.len() - 1)]
            .iter()
            .rev()
            .fold(None, |acc, next| acc.or_else(|| next.get(name)))
    }
    pub fn variables_at_depth(&mut self, depth: usize) -> &mut VariableMemories<'code> {
        // depth is not going to be an arbitrary amount longer, this just has
        // to cover the case when we increment the depth of the blocks

        while depth >= self.memories.len() {
            self.memories.push(VariableMemories::default())
        }
        &mut self.memories[depth]
    }
}

impl<'code> Default for VariableTracker<'code> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Error, Debug)]
pub enum VarError {
    #[error("unknown variable: {0:?}")]
    UnknownVariable(String),
    #[error("variable {0:?} was already declared")]
    Redeclared(String),
    #[error("unknown function: {0:?}")]
    UnknownFunction(String),
}

#[derive(Error, Debug)]
pub enum StatementError {
    #[error("{0}")]
    Variable(#[from] VarError),
    #[error("cannot continue if not inside a loop")]
    UnwantedContinue,
    #[error("cannot break if not inside a loop")]
    UnwantedBreak,
}

type StE = error::Error<StatementError>;
type VarE = error::Error<VarError>;
