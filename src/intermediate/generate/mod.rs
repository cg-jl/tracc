use std::collections::HashMap;
use std::mem::MaybeUninit;

use super::{
    BasicBlock, Binding, BlockBinding, ByteSize, Condition, CouldBeConstant, Statement, Value,
};
use crate::error::SourceMetadata;
use crate::grammar::lexer::Source;
use crate::intermediate::{BlockEnd, Branch, PhiDescriptor};
use crate::output::Output;
use crate::output_impl_From;
use crate::{ast, error};
mod block;
mod expr;
mod statement;
use thiserror::Error;

// TODO: make block builder struct
// TODO: make generators accept the current (unfinished) block as BlockBinding and return a
// BlockBinding (same block if not

pub struct BlockBuilder {
    current_block: Vec<Statement>,
    block_id: usize,
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

// enum BlockResult {
//     FinishedBlock(BlockBinding),
//     Unfinished(BlockBuilder),
// }

#[derive(Default)]
pub struct IRGenState {
    blocks: Vec<MaybeUninit<BasicBlock>>,
    pushed_blocks: usize,
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
        BlockBinding(block_id)
    }
    // get a new block and a reference to it
    fn new_block(&mut self) -> BlockBuilder {
        let index = self.blocks.len();
        self.blocks.push(MaybeUninit::uninit());
        BlockBuilder::new(index)
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
    pub fn get(&self, name: &str) -> Option<&(Binding, ByteSize)> {
        self.memories
            .iter()
            .rev()
            .fold(None, |acc, next| acc.or_else(|| next.get(name)))
    }
    pub fn variables_at_depth(&mut self, depth: usize) -> &mut VariableMemories<'code> {
        // depth is not going to be an arbitrary amount longer, this just has
        // to cover the case when we increment the depth of the blocks
        if depth > self.memories.len() {
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
