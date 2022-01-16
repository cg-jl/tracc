use super::{Binding, BlockEnd, Branch, CouldBeConstant, Value};

impl const From<Branch> for BlockEnd {
    fn from(br: Branch) -> Self {
        Self::Branch(br)
    }
}

impl const From<Binding> for CouldBeConstant {
    fn from(binding: Binding) -> Self {
        Self::Binding(binding)
    }
}

impl const From<u64> for CouldBeConstant {
    fn from(value: u64) -> Self {
        Self::Constant(value)
    }
}

impl const From<Binding> for Value {
    fn from(b: Binding) -> Self {
        Self::Binding(b)
    }
}

impl const From<u64> for Value {
    fn from(c: u64) -> Self {
        Self::Constant(c)
    }
}
