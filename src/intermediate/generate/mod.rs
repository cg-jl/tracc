use std::collections::HashMap;
use std::fmt;
use std::mem::MaybeUninit;

use super::{
    BasicBlock, Binding, BlockBinding, BranchingMap, ByteSize, Condition, IRCode, Statement, Value,
    IR,
};
use crate::error::SourceMetadata;
use crate::grammar::lexer::Source;
use crate::intermediate::{BlockEnd, Branch, PhiDescriptor};
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

impl From<Vec<BasicBlock>> for IR {
    fn from(code: Vec<BasicBlock>) -> Self {
        let (forward_map, backwards_map) = generate_branching_graphs(&code);
        Self {
            forward_map,
            backwards_map,
            code,
        }
    }
}

pub fn compile_function<'code>(
    f: ast::Function<'code>,
    source_meta: &SourceMetadata<'code>,
) -> Result<(&'code str, IR), VarE> {
    let ast::Function {
        name: ast::Identifier(name),
        body: ast::Block { statements },
    } = f;
    let mut state = IRGenState::default();
    let mut binding_counter = BindingCounter::default();
    let mut env = VariableTracker::default();
    let entry = state.new_block();
    let mut end = block::compile_block(
        &mut state,
        entry,
        statements,
        &mut binding_counter,
        &mut env,
        0,
        source_meta,
    )?;
    let ret = binding_counter.next_binding();
    end.assign(ret, 0);
    end.finish_block(&mut state, ret);
    let ir: IRCode = state.release().collect();
    let (forward_map, backwards_map) = generate_branching_graphs(&ir);

    let mut ir = IR {
        code: ir,
        backwards_map,
        forward_map,
    };

    // run some cleanup on the generated code, because we might have generated
    // too much garbage
    super::cleanup::run_safe_cleanup(&mut ir);
    super::cleanup::prune_unreached_blocks(&mut ir);

    Ok((name, ir))
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
    latest_binding: usize,
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
        self.memories[..=depth]
            .iter()
            .rev()
            .fold(None, |acc, next| acc.or_else(|| next.get(name)))
    }
    pub fn variables_at_depth(&mut self, depth: usize) -> &mut VariableMemories<'code> {
        // depth is not going to be an arbitrary amount longer, this just has
        // to cover the case when we increment the depth of the blocks
        if depth == self.memories.len() {
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
}

type VarE = error::Error<VarError>;
