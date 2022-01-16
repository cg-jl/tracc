use crate::codegen::assembly::Condition;
use crate::{format_instr, format_instr_args, write_instruction};
use std::fmt;
mod format;
mod generate;
mod convert;
// IR: everything is divided into basic blocks

pub type IR = Vec<BasicBlock>;

pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub end: BlockEnd,
}

#[derive(Clone, Copy)]
pub struct BlockBinding(pub usize);

#[derive(Clone, Copy)]
pub enum BlockEnd {
    Branch(Branch),
    Return { index: Binding },
}

#[derive(Clone, Copy)]
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

// phi, cmp, add, sub, neg.... all operations
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
        rhs: CouldBeConstant,
    },
    Divide {
        lhs: Binding,
        rhs: CouldBeConstant,
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
    // Constant value
    Constant(u64),
    // Other binding. Used by frontend, then cleaned up by next stage
    Binding(Binding),
}

#[derive(Clone, Copy)]
pub struct Binding(pub usize);

#[derive(Clone, Copy)]
pub enum CouldBeConstant {
    Binding(Binding),
    Constant(u64),
}

#[derive(Clone, Copy)]
pub enum ByteSize {
    U8,
    U32,
    U64,
}

#[derive(Clone, Copy)]
pub struct PhiDescriptor {
    pub value: CouldBeConstant,
    pub block_from: BlockBinding,
}


