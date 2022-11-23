use super::assembly;
use std::collections::VecDeque;
use std::fmt;

#[derive(Clone)]
pub struct AssemblyOutput<'code>(pub VecDeque<assembly::Assembly<'code>>);
impl Default for AssemblyOutput<'_> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'code> AssemblyOutput<'code> {
    pub fn new() -> Self {
        Self(VecDeque::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn into_inner(self) -> VecDeque<assembly::Assembly<'code>> {
        self.0
    }

    pub fn cons(mut self, value: impl Into<assembly::Assembly<'code>>) -> Self {
        self.push_front(value);
        self
    }

    pub fn push_front(&mut self, value: impl Into<assembly::Assembly<'code>>) -> &mut Self {
        self.0.push_front(value.into());
        self
    }

    pub fn push_back(&mut self, value: impl Into<assembly::Assembly<'code>>) -> &mut Self {
        self.0.push_back(value.into());
        self
    }

    pub fn chain_back<T>(mut self, values: impl IntoIterator<Item = T>) -> Self
    where
        T: Into<assembly::Assembly<'code>>,
    {
        for v in values {
            self.0.push_front(v.into());
        }
        self
    }

    pub fn chain_one(mut self, value: impl Into<assembly::Assembly<'code>>) -> Self {
        self.push_back(value);
        self
    }

    pub fn chain<T>(mut self, values: impl IntoIterator<Item = T>) -> Self
    where
        T: Into<assembly::Assembly<'code>>,
    {
        self.extend(values);
        self
    }

    pub fn extend<T>(&mut self, values: impl IntoIterator<Item = T>) -> &mut Self
    where
        T: Into<assembly::Assembly<'code>>,
    {
        self.0.extend(values.into_iter().map(T::into));
        self
    }

    pub fn iter_mut<'out>(
        &'out mut self,
    ) -> impl Iterator<Item = &'out mut assembly::Assembly<'code>> {
        self.0.iter_mut()
    }
}
impl<'code> IntoIterator for AssemblyOutput<'code> {
    type Item = assembly::Assembly<'code>;
    type IntoIter = <VecDeque<assembly::Assembly<'code>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'code, T> From<T> for AssemblyOutput<'code>
where
    T: Into<assembly::Assembly<'code>>,
{
    fn from(item: T) -> Self {
        Self::new().cons(item)
    }
}
impl<'code> FromIterator<AssemblyOutput<'code>> for AssemblyOutput<'code> {
    fn from_iter<T: IntoIterator<Item = AssemblyOutput<'code>>>(iter: T) -> Self {
        iter.into_iter()
            .fold(AssemblyOutput::new(), AssemblyOutput::chain)
    }
}
impl<'code> FromIterator<assembly::Assembly<'code>> for AssemblyOutput<'code> {
    fn from_iter<T: IntoIterator<Item = assembly::Assembly<'code>>>(iter: T) -> Self {
        Self(VecDeque::from_iter(iter))
    }
}

impl fmt::Debug for AssemblyOutput<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct DoDisplay<'a, 'code>(&'a assembly::Assembly<'code>);

        impl<'a, 'code> fmt::Debug for DoDisplay<'a, 'code> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        f.debug_list()
            .entries(self.0.iter().map(DoDisplay))
            .finish()
    }
}
