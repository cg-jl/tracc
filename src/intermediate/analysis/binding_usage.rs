use crate::intermediate::{BasicBlock, BlockEnd, Branch, IR};
use crate::intermediate::{Binding, CouldBeConstant, Statement, Value};
use std::collections::HashMap;
use std::iter::FromIterator;

#[derive(Debug, Clone, Copy)]
pub enum Usage {
    /// the binding is used in a computation of other binding
    Binding(Binding),
    /// the binding is used in a store
    Store(Binding),
    /// the binding is used in a return statement
    Return,
    /// the binding is used as a switch for branch
    Branch,
}
pub type UsageMap = HashMap<Binding, Vec<Usage>>;

// get what each binding is used for
pub fn get_usage_map(ir: &IR) -> UsageMap {
    let mut usage_map = UsageMap::new();

    for block in &ir.code {
        for statement in &block.statements {
            match statement {
                Statement::Store {
                    mem_binding,
                    binding,
                    ..
                } => usage_map
                    .entry(*binding)
                    .or_default()
                    .push(Usage::Store(*mem_binding)),
                Statement::Assign { index, value } => {
                    for dep in value.binding_deps() {
                        usage_map
                            .entry(dep)
                            .or_default()
                            .push(Usage::Binding(*index));
                    }
                }
            }
        }

        match block.end {
            BlockEnd::Return(ret) => usage_map.entry(ret).or_default().push(Usage::Return),
            BlockEnd::Branch(Branch::Conditional { flag, .. }) => {
                usage_map.entry(flag).or_default().push(Usage::Branch)
            }
            BlockEnd::Branch(Branch::Unconditional { .. }) => {}
        }
    }
    usage_map
}

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

impl BindingUsage for BasicBlock {
    fn binding_deps(&self) -> Vec<Binding> {
        let mut output: Vec<_> = self
            .statements
            .iter()
            .flat_map(|statement| statement.binding_deps())
            .collect();
        match self.end {
            BlockEnd::Branch(Branch::Conditional { flag, .. }) => output.push(flag),
            BlockEnd::Return(ret) => output.push(ret),
            _ => (),
        }
        output
    }
    fn contains_binding(&self, binding: Binding) -> bool {
        let is_in_end = match self.end {
            BlockEnd::Branch(Branch::Conditional { flag, .. }) => flag == binding,
            BlockEnd::Return(ret) => ret == binding,
            _ => false,
        };
        is_in_end
            || (self
                .statements
                .iter()
                .any(|statement| statement.contains_binding(binding)))
    }
}
