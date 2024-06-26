use std::fmt;

use crate::write_instruction;

use super::{
    BasicBlock, Binding, BlockBinding, BlockEnd, Branch, ByteSize, CouldBeConstant, PhiDescriptor,
    Statement, Value, IR,
};

// format impls
impl fmt::Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl fmt::Debug for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

impl fmt::Display for BlockBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BB{}", self.0)
    }
}

impl fmt::Debug for BlockBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
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
        write!(f, "[ {}, {} ]", self.value, self.block_from)
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
            Value::Call { label, args } => {
                f.write_str("call ")?;
                label.fmt(f)?;
                for arg in args {
                    f.write_str(", ")?;
                    arg.fmt(f)?;
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
            } => write_instruction!(f, if *is_signed { "idiv" } else { "udiv" }, lhs, rhs),
            Value::Lsl { lhs, rhs } => write_instruction!(f, "lsl", lhs, rhs),
            Value::Lsr { lhs, rhs } => write_instruction!(f, "lsr", lhs, rhs),
            Value::And { lhs, rhs } => write_instruction!(f, "and", lhs, rhs),
            Value::Or { lhs, rhs } => write_instruction!(f, "or", lhs, rhs),
            Value::Xor { lhs, rhs } => write_instruction!(f, "xor", lhs, rhs),
            Value::Multiply { lhs, rhs } => write_instruction!(f, "mul", lhs, rhs),
            Value::Allocate { size } => write_instruction!(f, "alloca", size),
            Value::Constant(constant) => constant.fmt(f),
            Value::Binding(binding) => binding.fmt(f),
            Value::Uninit => f.write_str("uninit"),
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

impl fmt::Display for BlockEnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockEnd::Branch(br) => br.fmt(f),
            BlockEnd::Return(index) => write_instruction!(f, "ret", index),
        }
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\n")?;
        for stmt in &self.statements {
            writeln!(f, "  {}", stmt)?;
        }
        writeln!(f, "  {}", self.end)
    }
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Branch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Branch::Unconditional { target } => write_instruction!(f, "br", target),
            Branch::Conditional {
                flag,
                target_true,
                target_false,
            } => write_instruction!(f, "br-cond", flag, target_true, target_false),
        }
    }
}

impl fmt::Debug for IR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("\n")?;
        for (block_index, block) in self.code.iter().enumerate() {
            let bb = BlockBinding(block_index);
            write!(f, "{}:", bb)?;
            write!(f, "{}", block)?;
        }
        Ok(())
    }
}
