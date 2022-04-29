use super::assembly;
use std::collections::VecDeque;

pub struct AssemblyOutput(VecDeque<assembly::Assembly>);
impl Default for AssemblyOutput {
    fn default() -> Self {
        Self::new()
    }
}
impl AssemblyOutput {
    pub fn new() -> Self {
        Self(VecDeque::new())
    }

    pub fn cons(mut self, value: impl Into<assembly::Assembly>) -> Self {
        self.push_front(value);
        self
    }

    pub fn push_front(&mut self, value: impl Into<assembly::Assembly>) -> &mut Self {
        self.0.push_front(value.into());
        self
    }

    pub fn push_back(&mut self, value: impl Into<assembly::Assembly>) -> &mut Self {
        self.0.push_back(value.into());
        self
    }

    pub fn chain_back<T>(mut self, values: impl IntoIterator<Item = T>) -> Self
    where
        T: Into<assembly::Assembly>,
    {
        for v in values {
            self.0.push_front(v.into());
        }
        self
    }

    pub fn chain_one(mut self, value: impl Into<assembly::Assembly>) -> Self {
        self.push_back(value);
        self
    }

    pub fn chain<T>(mut self, values: impl IntoIterator<Item = T>) -> Self
    where
        T: Into<assembly::Assembly>,
    {
        self.extend(values);
        self
    }

    pub fn extend<T>(&mut self, values: impl IntoIterator<Item = T>) -> &mut Self
    where
        T: Into<assembly::Assembly>,
    {
        self.0.extend(values.into_iter().map(T::into));
        self
    }

    pub fn iter_mut<'out>(&'out mut self) -> impl Iterator<Item = &'out mut assembly::Assembly> {
        self.0.iter_mut()
    }
}
impl IntoIterator for AssemblyOutput {
    type Item = assembly::Assembly;
    type IntoIter = <VecDeque<assembly::Assembly> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<T> From<T> for AssemblyOutput
where
    T: Into<assembly::Assembly>,
{
    fn from(item: T) -> Self {
        Self::new().cons(item)
    }
}
impl FromIterator<AssemblyOutput> for AssemblyOutput {
    fn from_iter<T: IntoIterator<Item = AssemblyOutput>>(iter: T) -> Self {
        iter.into_iter()
            .fold(AssemblyOutput::new(), AssemblyOutput::chain)
    }
}
impl FromIterator<assembly::Assembly> for AssemblyOutput {
    fn from_iter<T: IntoIterator<Item = assembly::Assembly>>(iter: T) -> Self {
        Self(VecDeque::from_iter(iter))
    }
}
