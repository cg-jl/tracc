use std::collections::HashMap;

pub mod analysis;
pub mod cleanup;
mod convert;
pub mod fold;
mod format;
pub mod generate;
pub mod refactor;

use crate::asmgen::assembly::Condition;

use self::analysis::lifetimes::BlockAddress;
// IR: everything is divided into basic blocks

pub type BranchingMap = HashMap<BlockBinding, Vec<BlockBinding>>;

pub type IRCode = Vec<BasicBlock>;

#[derive(Clone, Copy)]
pub struct BlockRange {
    pub start: BlockBinding,
    pub end: BlockBinding,
}

pub struct IR {
    pub code: IRCode,
    pub backwards_map: BranchingMap,
    pub forward_map: BranchingMap,
    pub function_entrypoints: Vec<BlockBinding>,
    pub function_block_ranges: Vec<BlockRange>,
    pub function_argument_bindings: Vec<core::ops::Range<usize>>,
}

impl IR {
    pub fn get_statement(&self, addr: &BlockAddress) -> Option<&Statement> {
        self[addr.block].statements.get(addr.statement)
    }
}

pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub end: BlockEnd,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct BlockBinding(pub usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BlockEnd {
    Branch(Branch),
    Return(Binding),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Branch {
    Unconditional {
        target: BlockBinding,
    },
    Conditional {
        flag: Binding,
        target_true: BlockBinding,
        target_false: BlockBinding,
    },
}

// assign, store, load, alloc, free
#[derive(Debug, PartialEq)]
pub enum Statement {
    Assign {
        index: Binding,
        value: Value,
    },
    Store {
        mem_binding: Binding,
        binding: Binding,
        byte_size: ByteSize,
    },
}

// TODO: merge binary ops from `Value` into the same value kind, same for unops

// phi, cmp, add, sub, neg.... all operations
#[derive(Debug, PartialEq)]
pub enum Value {
    // allocate memory
    Allocate {
        size: usize,
    },
    Phi {
        nodes: Vec<PhiDescriptor>,
    },
    Cmp {
        condition: Condition,
        lhs: Binding,
        rhs: CouldBeConstant,
    },
    // load from memory 1-4-8 bytes
    Load {
        mem_binding: Binding,
        byte_size: ByteSize,
    },
    // -x
    Negate {
        binding: Binding,
    },
    // ~x
    FlipBits {
        binding: Binding,
    },
    Add {
        lhs: Binding,
        rhs: CouldBeConstant,
    },
    Subtract {
        lhs: Binding,
        rhs: CouldBeConstant,
    },
    Multiply {
        lhs: Binding,
        rhs: Binding,
    },
    // NOTE: `rhs` should be a binding since `udiv` only accepts registers.
    Divide {
        lhs: Binding,
        rhs: Binding,
        is_signed: bool,
    },
    // logic shift left
    Lsl {
        lhs: Binding,
        rhs: CouldBeConstant,
    },
    // Logic shift right
    Lsr {
        lhs: Binding,
        rhs: CouldBeConstant,
    },
    // bitwise AND
    And {
        lhs: Binding,
        rhs: CouldBeConstant,
    },
    // bitwise OR
    Or {
        lhs: Binding,
        rhs: CouldBeConstant,
    },
    // bitwise XOR
    Xor {
        lhs: Binding,
        rhs: CouldBeConstant,
    },
    // Call label with arguments
    Call {
        label: BlockBinding,
        args: Vec<Binding>,
    },
    // Constant value
    Constant(i32),
    // Other binding. Used by frontend, then cleaned up by next stage
    Binding(Binding),
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Binding(pub usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CouldBeConstant {
    Binding(Binding),
    Constant(i32),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ByteSize {
    U8,
    U32,
    U64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PhiDescriptor {
    pub value: Binding,
    pub block_from: BlockBinding,
}

impl CouldBeConstant {
    pub const fn as_binding(self) -> Option<Binding> {
        match self {
            CouldBeConstant::Binding(binding) => Some(binding),
            CouldBeConstant::Constant(_) => None,
        }
    }
}

impl BlockEnd {
    pub fn branch_list(&self) -> impl Iterator<Item = BlockBinding> + '_ {
        match self {
            BlockEnd::Branch(branch) => Some(branch.branch_list()),
            BlockEnd::Return(_) => None,
        }
        .into_iter()
        .flatten()
    }
}

impl Branch {
    pub fn branch_list(&self) -> impl Iterator<Item = BlockBinding> + '_ {
        let mut iteration = 0usize;
        std::iter::from_fn(move || {
            let last_iteration = iteration;
            iteration += 1;
            match self {
                Branch::Unconditional { target } => {
                    if last_iteration == 0 {
                        Some(*target)
                    } else {
                        None
                    }
                }
                Branch::Conditional {
                    flag: _,
                    target_true,
                    target_false,
                } => match last_iteration {
                    0 => Some(*target_true),
                    1 => Some(*target_false),
                    _ => None,
                },
            }
        })
    }
}

impl core::ops::Index<analysis::lifetimes::BlockAddress> for IR {
    type Output = Statement;

    fn index(&self, index: analysis::lifetimes::BlockAddress) -> &Self::Output {
        &self[index.block].statements[index.statement]
    }
}

impl core::ops::Index<BlockBinding> for IR {
    type Output = BasicBlock;
    fn index(&self, index: BlockBinding) -> &Self::Output {
        &self.code[index.0]
    }
}

impl core::ops::IndexMut<BlockBinding> for IR {
    fn index_mut(&mut self, index: BlockBinding) -> &mut Self::Output {
        &mut self.code[index.0]
    }
}
