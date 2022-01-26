use crate::intermediate::{
    BasicBlock, Binding, BlockEnd, Branch, CouldBeConstant, PhiDescriptor, Statement, Value,
};

pub trait BindingUsage {
    fn uses_binding(&self, binding: Binding) -> bool;
}

impl BindingUsage for CouldBeConstant {
    fn uses_binding(&self, binding: Binding) -> bool {
        if let Some(b) = self.as_binding() {
            b == binding
        } else {
            false
        }
    }
}

impl BindingUsage for PhiDescriptor {
    fn uses_binding(&self, binding: Binding) -> bool {
        self.value.uses_binding(binding)
    }
}

impl BindingUsage for Binding {
    fn uses_binding(&self, binding: Binding) -> bool {
        self == &binding
    }
}

impl BindingUsage for Value {
    fn uses_binding(&self, binding: Binding) -> bool {
        match self {
            Value::Allocate { .. } => false,
            Value::Phi { nodes } => nodes.iter().any(|node| node.uses_binding(binding)),
            Value::Cmp {
                condition: _,
                lhs,
                rhs,
            }
            | Value::Add { lhs, rhs }
            | Value::Subtract { lhs, rhs }
            | Value::Multiply { lhs, rhs }
            | Value::Lsl { lhs, rhs }
            | Value::Lsr { lhs, rhs }
            | Value::And { lhs, rhs }
            | Value::Or { lhs, rhs }
            | Value::Divide {
                lhs,
                rhs,
                is_signed: _,
            }
            | Value::Xor { lhs, rhs } => lhs.uses_binding(binding) || rhs.uses_binding(binding),
            Value::Load {
                mem_binding,
                byte_size: _,
            } => mem_binding.uses_binding(binding),
            Value::Negate { binding: other } | Value::FlipBits { binding: other } => {
                other.uses_binding(binding)
            }
            Value::Constant(_) => todo!(),
            Value::Binding(other) => other.uses_binding(binding),
        }
    }
}

impl BindingUsage for Statement {
    fn uses_binding(&self, other: Binding) -> bool {
        match self {
            Statement::Assign { index: _, value } => value.uses_binding(other),
            Statement::Store {
                mem_binding,
                binding,
                byte_size: _,
            } => mem_binding.uses_binding(other) || binding.uses_binding(other),
        }
    }
}

impl BindingUsage for BlockEnd {
    fn uses_binding(&self, binding: Binding) -> bool {
        match self {
            BlockEnd::Branch(branch) => branch.uses_binding(binding),
            BlockEnd::Return(ret) => ret.uses_binding(binding),
        }
    }
}

impl BindingUsage for Branch {
    fn uses_binding(&self, binding: Binding) -> bool {
        if let Branch::Conditional {
            flag,
            target_true: _,
            target_false: _,
        } = self
        {
            flag.uses_binding(binding)
        } else {
            false
        }
    }
}

impl BindingUsage for BasicBlock {
    fn uses_binding(&self, binding: Binding) -> bool {
        self.end.uses_binding(binding) || self.statements.iter().any(|x| x.uses_binding(binding))
    }
}
