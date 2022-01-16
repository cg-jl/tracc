use crate::codegen::assembly::Condition;
use crate::{format_instr, format_instr_args, write_instruction};
use std::fmt;
mod generate;
// IR: everything is divided into basic blocks

pub type IR = Vec<BasicBlock>;

#[derive(Default)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub end: Option<BlockEnd>,
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
    pub variable: Binding,
    pub block_from: BlockBinding,
}

impl fmt::Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl fmt::Display for BlockBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BB{}", self.0)
    }
}

impl fmt::Display for CouldBeConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CouldBeConstant::Binding(binding) => binding.fmt(f),
            CouldBeConstant::Constant(constant) => constant.fmt(f),
        }
    }
}

impl fmt::Display for PhiDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[ {}, {} ]", self.variable, self.block_from)
    }
}

impl fmt::Display for ByteSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::U8 => "u8",
            Self::U32 => "u32",
            Self::U64 => "u64",
        })
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Phi { nodes } => {
                f.write_str("phi ")?;
                nodes[0].fmt(f)?; // phi nodes need at least one value
                for node in &nodes[1..] {
                    f.write_str(", ")?;
                    node.fmt(f)?;
                }
                Ok(())
            }
            Value::Cmp {
                condition,
                lhs,
                rhs,
            } => write_instruction!(f, "cmp", condition, lhs, rhs),
            Value::Load {
                mem_binding,
                byte_size,
            } => write_instruction!(f, "load", mem_binding, byte_size),
            Value::Negate { binding } => write_instruction!(f, "neg", binding),
            Value::FlipBits { binding } => write_instruction!(f, "flip_bits", binding),
            Value::Add { lhs, rhs } => write_instruction!(f, "add", lhs, rhs),
            Value::Subtract { lhs, rhs } => write_instruction!(f, "sub", lhs, rhs),
            Value::Divide {
                lhs,
                rhs,
                is_signed,
            } => write_instruction!(f, if *is_signed { 'u' } else { 's' }, lhs, rhs),
            Value::Lsl { lhs, rhs } => write_instruction!(f, "lsl", lhs, rhs),
            Value::Lsr { lhs, rhs } => write_instruction!(f, "lsr", lhs, rhs),
            Value::And { lhs, rhs } => write_instruction!(f, "and", lhs, rhs),
            Value::Or { lhs, rhs } => write_instruction!(f, "or", lhs, rhs),
            Value::Xor { lhs, rhs } => write_instruction!(f, "xor", lhs, rhs),
            Value::Multiply { lhs, rhs } => write_instruction!(f, "mul", lhs, rhs),
            Value::Allocate { size } => write_instruction!(f, "alloca", size),
            Value::Constant(constant) => constant.fmt(f),
            Value::Binding(binding) => binding.fmt(f),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Assign { index, value } => write!(f, "{} = {}", index, value),
            Statement::Store {
                mem_binding,
                binding,
                byte_size,
            } => write_instruction!(
                f,
                "store",
                mem_binding,
                format!("{} {}", byte_size, binding)
            ),
        }
    }
}
