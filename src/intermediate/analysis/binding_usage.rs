use std::iter::FromIterator;

use crate::intermediate::{Binding, CouldBeConstant, Statement, Value};

pub trait BindingUsage {
    // Check if a binding is used
    fn contains_binding(&self, binding: Binding) -> bool;
    // Get all the dependencies
    fn binding_deps(&self) -> Vec<Binding>;
}

impl BindingUsage for Binding {
    fn contains_binding(&self, binding: Binding) -> bool {
        self == &binding
    }

    fn binding_deps(&self) -> Vec<Binding> {
        vec![*self]
    }
}

impl BindingUsage for CouldBeConstant {
    fn contains_binding(&self, binding: Binding) -> bool {
        match self {
            CouldBeConstant::Binding(b) => b.contains_binding(binding),
            CouldBeConstant::Constant(_) => false,
        }
    }

    fn binding_deps(&self) -> Vec<Binding> {
        Vec::from_iter(self.as_binding())
    }
}

impl BindingUsage for Value {
    fn contains_binding(&self, search_target: Binding) -> bool {
        match self {
            Value::Allocate { size: _ } => false,
            Value::Phi { nodes } => nodes
                .iter()
                .any(|node| node.value.contains_binding(search_target)),
            Value::Add { lhs, rhs }
            | Value::Subtract { lhs, rhs }
            | Value::Multiply { lhs, rhs }
            | Value::Divide {
                lhs,
                rhs,
                is_signed: _,
            }
            | Value::Lsl { lhs, rhs }
            | Value::Lsr { lhs, rhs }
            | Value::And { lhs, rhs }
            | Value::Or { lhs, rhs }
            | Value::Xor { lhs, rhs }
            | Value::Cmp {
                condition: _,
                lhs,
                rhs,
            } => lhs.contains_binding(search_target) | rhs.contains_binding(search_target),
            Value::Load {
                mem_binding,
                byte_size: _,
            } => mem_binding.contains_binding(search_target),
            Value::FlipBits { binding } | Value::Negate { binding } => {
                binding.contains_binding(search_target)
            }
            Value::Constant(_) => false,
            Value::Binding(b) => b.contains_binding(search_target),
        }
    }

    fn binding_deps(&self) -> Vec<Binding> {
        match self {
            Value::Constant(_) | Value::Allocate { size: _ } => vec![],
            Value::Phi { nodes } => nodes
                .iter()
                .flat_map(|node| node.value.as_binding())
                .collect(),
            Value::Add { lhs, rhs }
            | Value::Subtract { lhs, rhs }
            | Value::Multiply { lhs, rhs }
            | Value::Divide {
                lhs,
                rhs,
                is_signed: _,
            }
            | Value::Lsl { lhs, rhs }
            | Value::Lsr { lhs, rhs }
            | Value::And { lhs, rhs }
            | Value::Or { lhs, rhs }
            | Value::Xor { lhs, rhs }
            | Value::Cmp {
                condition: _,
                lhs,
                rhs,
            } => Some(*lhs).into_iter().chain(rhs.as_binding()).collect(),
            Value::Load {
                mem_binding,
                byte_size: _,
            } => vec![*mem_binding],
            Value::Binding(binding) | Value::Negate { binding } | Value::FlipBits { binding } => {
                vec![*binding]
            }
        }
    }
}

impl BindingUsage for Statement {
    fn binding_deps(&self) -> Vec<Binding> {
        match self {
            Self::Store {
                byte_size: _,
                mem_binding,
                binding,
            } => vec![*mem_binding, *binding],
            Self::Assign { index: _, value } => value.binding_deps(),
        }
    }
    fn contains_binding(&self, target: Binding) -> bool {
        match self {
            Self::Store {
                mem_binding,
                binding,
                byte_size: _,
            } => mem_binding.contains_binding(target) || binding.contains_binding(target),
            Self::Assign { index: _, value } => value.contains_binding(target),
        }
    }
}
