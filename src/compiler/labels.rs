use crate::assembly::Label;

static mut GENERATOR: LabelGenerator = LabelGenerator::new();

pub struct LabelGenerator {
    current: usize,
}

impl LabelGenerator {
    pub fn global() -> &'static mut Self {
        // UNSAFE: we give a &mut ref, meaning that
        // no other thing can access it while it's being modified.
        // we never give a non-mutable access because there's no need to.
        unsafe { &mut GENERATOR }
    }
    const fn new() -> Self {
        Self { current: 0 }
    }
    pub fn new_label(&mut self) -> Label {
        let current = self.current;
        self.current += 1;
        // UNSAFE: the label is ensured to be
        // unique, as the only way to instantiate this from outside
        // is through the static mut variable.
        unsafe { Label::new(current) }
    }
}
