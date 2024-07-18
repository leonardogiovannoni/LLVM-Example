
use std::{fmt::{Debug, Formatter}, ops::Deref, rc::Rc};
use std::hash::{Hash, Hasher};
// [begin, end)
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



pub struct RcStr {
    pub s: Rc<str>,
    pub span: Span,
}

impl Debug for RcStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl PartialEq for RcStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Deref for RcStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl Eq for RcStr {}

impl Hash for RcStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl RcStr {
    fn new(s: Rc<str>, span: Span) -> Self {
        Self { s, span }
    }

    fn as_str(&self) -> &str {
        &self.s[self.span.begin..self.span.end]
    }
}
