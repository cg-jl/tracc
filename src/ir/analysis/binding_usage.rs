use crate::ir::{BasicBlock, BlockBinding, BlockEnd, Branch, PhiDescriptor, IR};
use crate::ir::{Binding, CouldBeConstant, Statement, Value};
use core::ops::ControlFlow;
use std::collections::HashMap;
use std::iter::FromIterator;

use super::lifetimes::BlockAddress;

#[derive(Debug)]
pub struct Usage {
    pub addr: BlockAddress,
    pub binding: Binding,
}

pub fn visit_value_bindings<B>(
    ir: &IR,
    mut visit: impl FnMut(Usage) -> ControlFlow<B>,
) -> ControlFlow<B> {
    for (bb, block) in super::iterate_with_bindings(&ir.code) {
        for (stmt_index, stmt) in block.statements.iter().enumerate() {
            stmt.visit_value_bindings(|binding| {
                visit(Usage {
                    binding,
                    addr: BlockAddress {
                        block: bb,
                        statement: stmt_index,
                    },
                })
            })?;
        }

        match block.end {
            BlockEnd::Branch(branch) => {
                if let Branch::Conditional { flag, .. } = branch {
                    visit(Usage {
                        binding: flag,
                        addr: BlockAddress {
                            block: bb,
                            statement: block.statements.len(),
                        },
                    })?;
                }
            }
            BlockEnd::Return(binding) => {
                visit(Usage {
                    binding,
                    addr: BlockAddress {
                        block: bb,
                        statement: block.statements.len(),
                    },
                })?;
            }
        }
    }
    ControlFlow::Continue(())
}

// get what each binding is used for

pub trait BindingUsage {
    // Check if a binding is used
    fn contains_binding(&self, binding: Binding) -> bool;
    // Get all the dependencies
    fn visit_value_bindings<B, F: FnMut(Binding) -> ControlFlow<B>>(&self, f: F) -> ControlFlow<B>;
}

impl BindingUsage for Binding {
    fn contains_binding(&self, binding: Binding) -> bool {
        self == &binding
    }

    fn visit_value_bindings<B, F: FnMut(Binding) -> ControlFlow<B>>(
        &self,
        mut f: F,
    ) -> ControlFlow<B> {
        f(*self)
    }
}

impl BindingUsage for CouldBeConstant {
    fn contains_binding(&self, binding: Binding) -> bool {
        match self {
            CouldBeConstant::Binding(b) => b.contains_binding(binding),
            CouldBeConstant::Constant(_) => false,
        }
    }

    fn visit_value_bindings<B, F: FnMut(Binding) -> ControlFlow<B>>(
        &self,
        mut f: F,
    ) -> ControlFlow<B> {
        match self {
            CouldBeConstant::Binding(b) => b.visit_value_bindings(f),
            CouldBeConstant::Constant(_) => ControlFlow::Continue(()),
        }
    }
}

impl BindingUsage for Value {
    fn contains_binding(&self, search_target: Binding) -> bool {
        match self {
            Value::Call { args, .. } => args.iter().any(|b| b.contains_binding(search_target)),
            Value::Allocate { size: _ } => false,
            Value::Phi { nodes } => nodes
                .iter()
                .any(|node| node.value.contains_binding(search_target)),
            Value::Add { lhs, rhs }
            | Value::Subtract { lhs, rhs }
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
            Value::Multiply { lhs, rhs }
            | Value::Divide {
                lhs,
                rhs,
                is_signed: _,
            } => lhs.contains_binding(search_target) || rhs.contains_binding(search_target),
            Value::Constant(_) => false,
            Value::Binding(b) => b.contains_binding(search_target),
            Value::Uninit => false,
        }
    }

    fn visit_value_bindings<B, F: FnMut(Binding) -> ControlFlow<B>>(
        &self,
        mut f: F,
    ) -> ControlFlow<B> {
        match self {
            Value::Constant(_) | Value::Allocate { size: _ } => ControlFlow::Continue(()),
            Value::Phi { nodes } => nodes.iter().try_for_each(|node| f(node.value)),
            Value::Call { args, .. } => args.iter().copied().try_for_each(f),
            Value::Add { lhs, rhs }
            | Value::Subtract { lhs, rhs }
            | Value::Lsl { lhs, rhs }
            | Value::Lsr { lhs, rhs }
            | Value::And { lhs, rhs }
            | Value::Or { lhs, rhs }
            | Value::Xor { lhs, rhs }
            | Value::Cmp {
                condition: _,
                lhs,
                rhs,
            } => {
                f(*lhs)?;
                rhs.visit_value_bindings(f)
            }
            Value::Multiply { lhs, rhs }
            | Value::Divide {
                lhs,
                rhs,
                is_signed: _,
            } => {
                f(*lhs)?;
                f(*rhs)
            }
            Value::Load {
                mem_binding,
                byte_size: _,
            } => f(*mem_binding),
            Value::Binding(binding) | Value::Negate { binding } | Value::FlipBits { binding } => {
                f(*binding)
            }
            Value::Uninit => ControlFlow::Continue(()),
        }
    }
}

impl BindingUsage for Statement {
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

    fn visit_value_bindings<B, F: FnMut(Binding) -> ControlFlow<B>>(
        &self,
        mut f: F,
    ) -> ControlFlow<B> {
        match self {
            Self::Store {
                mem_binding,
                binding,
                byte_size: _,
            } => {
                f(*binding)?;
                f(*mem_binding)
            }
            Self::Assign { index: _, value } => value.visit_value_bindings(&mut f),
        }
    }
}

impl BindingUsage for BasicBlock {
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

    fn visit_value_bindings<B, F: FnMut(Binding) -> ControlFlow<B>>(
        &self,
        mut f: F,
    ) -> ControlFlow<B> {
        self.statements
            .iter()
            .try_for_each(|s| s.visit_value_bindings(&mut f))?;

        match &self.end {
            BlockEnd::Branch(Branch::Conditional { flag, .. }) => f(*flag),
            BlockEnd::Return(ret) => f(*ret),
            _ => ControlFlow::Continue(()),
        }
    }
}

impl BindingUsage for IR {
    fn contains_binding(&self, binding: Binding) -> bool {
        true
    }

    fn visit_value_bindings<B, F: FnMut(Binding) -> ControlFlow<B>>(
        &self,
        mut f: F,
    ) -> ControlFlow<B> {
        self.code
            .iter()
            .try_for_each(|block| block.visit_value_bindings(&mut f))
    }
}
