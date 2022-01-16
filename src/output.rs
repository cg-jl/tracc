use std::{collections::vec_deque::VecDeque, iter::FromIterator};

#[repr(transparent)]
#[derive(Debug, Default)]
pub struct Output<O> {
    inner: VecDeque<O>,
}

impl<O> Output<O> {
    // constructors
    /// Get a fresh, empty `Output<O>` structure. This is used as a fallback
    /// in case your type is not `Default`able.
    pub fn new() -> Self {
        Self {
            inner: VecDeque::new(),
        }
    }
    pub fn singleton(value: impl Into<O>) -> Self {
        Self::new().chain_single(value)
    }

    // functions taking &mut
    pub fn push(&mut self, value: impl Into<O>) {
        self.inner.push_back(value.into())
    }
    pub fn cons(&mut self, value: impl Into<O>) {
        self.inner.push_front(value.into())
    }
    pub fn iter_mut<'s>(&'s mut self) -> impl Iterator<Item = &'s mut O> {
        self.inner.iter_mut()
    }
    pub fn extend<T: Into<O>>(&mut self, iter: impl IntoIterator<Item = T>) {
        self.inner.extend(iter.into_iter().map(Into::into))
    }

    // similar functions to above but taking a moving self

    pub fn chain_single(mut self, value: impl Into<O>) -> Self {
        self.push(value);
        self
    }

    pub fn chain<T: Into<O>>(mut self, iter: impl IntoIterator<Item = T>) -> Self {
        self.extend(iter);
        self
    }
}

// this impl forwards to VecDeque's `IntoIterator`
impl<O> IntoIterator for Output<O> {
    type IntoIter = <VecDeque<O> as IntoIterator>::IntoIter;
    type Item = <Self::IntoIter as IntoIterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<O, T> FromIterator<T> for Output<O>
where
    T: Into<O>,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self {
            inner: VecDeque::from_iter(iter.into_iter().map(Into::into)),
        }
    }
}

// as I can't create the impl through generics (rust can't decide between two conflicting impls)
// I will generate them through a macro
#[macro_export]
macro_rules! output_impl_From {
    ($type:ident) => {
        impl<A: Into<$type>> From<A> for Output<$type> {
            fn from(v: A) -> Self {
                Self::singleton(v)
            }
        }
    };
}
