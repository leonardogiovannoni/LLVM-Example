use std::fmt::{Debug, Formatter};

/// span in the form of  [begin, end)
/// represents the *bytes* range of a string
#[derive(Clone, Copy)]
pub struct Span {
    pub begin: usize,
    pub end: usize,
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "Span({}..{})", self.begin, self.end)
    }
}

impl Span {
    pub fn empty() -> Self {
        Span { begin: 0, end: 0 }
    }
}
