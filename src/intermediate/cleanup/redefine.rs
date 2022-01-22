use crate::intermediate::{
    BasicBlock, Binding, BlockEnd, Branch, CouldBeConstant, PhiDescriptor, Statement, Value, IRCode,
};

/// Mechanism used by cleanup code to rename bindings
pub trait Rename {
    fn rename(&mut self, target: Binding, rename_as: Binding);
}

impl Rename for Binding {
    fn rename(&mut self, target: Binding, rename_as: Binding) {
        if self.0 == target.0 {
            *self = rename_as;
        }
    }
}

impl Rename for CouldBeConstant {
    fn rename(&mut self, target: Binding, rename_as: Binding) {
        match self {
            CouldBeConstant::Binding(binding) => binding.rename(target, rename_as),
            CouldBeConstant::Constant(_) => (),
        }
    }
}

impl Rename for PhiDescriptor {
    fn rename(&mut self, target: Binding, rename_as: Binding) {
        self.value.rename(target, rename_as)
    }
}

impl Rename for Value {
    fn rename(&mut self, target: Binding, rename_as: Binding) {
        match self {
            Value::Allocate { size: _ } => (),
            Value::Phi { nodes } => nodes
                .iter_mut()
                .for_each(|node| node.rename(target, rename_as)),
            Value::Cmp {
                condition: _,
                lhs,
                rhs,
            } => {
                lhs.rename(target, rename_as);
                rhs.rename(target, rename_as);
            }
            Value::Load {
                mem_binding,
                byte_size: _,
            } => {
                mem_binding.rename(target, rename_as);
            }

            Value::Negate { binding } | Value::FlipBits { binding } | Value::Binding(binding) => {
                binding.rename(target, rename_as)
            }
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
            | Value::Xor { lhs, rhs } => {
                lhs.rename(target, rename_as);
                rhs.rename(target, rename_as);
            }
            Value::Constant(_) => (),
        }
    }
}

impl Rename for Statement {
    fn rename(&mut self, target: Binding, rename_as: Binding) {
        match self {
            Statement::Assign { index: _, value } => value.rename(target, rename_as),
            Statement::Store {
                mem_binding,
                binding,
                byte_size: _,
            } => {
                mem_binding.rename(target, rename_as);
                binding.rename(target, rename_as);
            }
        }
    }
}

impl Rename for BlockEnd {
    fn rename(&mut self, target: Binding, rename_as: Binding) {
        match self {
            BlockEnd::Branch(branch) => branch.rename(target, rename_as),
            BlockEnd::Return(value) => value.rename(target, rename_as),
        }
    }
}

impl Rename for Branch {
    fn rename(&mut self, target: Binding, rename_as: Binding) {
        if let Branch::Conditional {
            flag,
            target_true: _,
            target_false: _,
        } = self
        {
            flag.rename(target, rename_as);
        }
    }
}

impl Rename for BasicBlock {
    fn rename(&mut self, target: Binding, rename_as: Binding) {
        for stmt in self.statements.iter_mut() {
            stmt.rename(target, rename_as);
        }
        self.end.rename(target, rename_as);
    }
}

impl Rename for IRCode {
    fn rename(&mut self, target: Binding, rename_as: Binding) {
        for block in self.iter_mut() {
            block.rename(target, rename_as);
        }
    }
}
