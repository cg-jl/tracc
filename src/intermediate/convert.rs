use super::{Binding, BlockEnd, Branch, CouldBeConstant, Value};

impl From<Branch> for BlockEnd {
    fn from(br: Branch) -> Self {
        Self::Branch(br)
    }
}

impl From<Binding> for BlockEnd {
    fn from(ret_value: Binding) -> Self {
        Self::Return(ret_value)
    }
}

impl From<Binding> for CouldBeConstant {
    fn from(binding: Binding) -> Self {
        Self::Binding(binding)
    }
}

impl From<i32> for CouldBeConstant {
    fn from(value: i32) -> Self {
        Self::Constant(value)
    }
}

impl From<Binding> for Value {
    fn from(b: Binding) -> Self {
        Self::Binding(b)
    }
}

impl From<i32> for Value {
    fn from(c: i32) -> Self {
        Self::Constant(c)
    }
}
