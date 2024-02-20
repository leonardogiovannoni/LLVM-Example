use triomphe::Arc;
use crate::RefSlice;
use std::rc::Rc;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RefStr {
    ref_slice: RefSlice<u8>,
}

impl std::fmt::Debug for RefStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.as_str(), f)
    }
}

pub trait RefStrIndex<'a, S: 'a> {
    type Output: 'a;

    fn get(self, s: &'a S) -> Option<Self::Output>;
}

impl<'a> RefStrIndex<'a, RefStr> for usize {
    type Output = u8;

    fn get(self, s: &'a RefStr) -> Option<Self::Output> {
        s.ref_slice.get(self).map(|&b| b)
    }
}

impl<'a> RefStrIndex<'a, RefStr> for std::ops::Range<usize> {
    type Output = RefStr;

    fn get(self, s: &'a RefStr) -> Option<Self::Output> {
        let inner = s.ref_slice.get(self).map(|s| s);
        inner.map(|s| RefStr { ref_slice: s })
    }
}

impl<'a> RefStrIndex<'a, RefStr> for std::ops::RangeTo<usize> {
    type Output = RefStr;

    fn get(self, s: &'a RefStr) -> Option<Self::Output> {
        let inner = s.ref_slice.get(self).map(|s| s);
        inner.map(|s| RefStr { ref_slice: s })
    }
}

impl<'a> RefStrIndex<'a, RefStr> for std::ops::RangeFrom<usize> {
    type Output = RefStr;

    fn get(self, s: &'a RefStr) -> Option<Self::Output> {
        let inner = s.ref_slice.get(self).map(|s| s);
        inner.map(|s| RefStr { ref_slice: s })
    }
}

impl<'a> RefStrIndex<'a, RefStr> for std::ops::RangeFull {
    type Output = RefStr;

    fn get(self, s: &'a RefStr) -> Option<Self::Output> {
        let inner = s.ref_slice.get(self).map(|s| s);
        inner.map(|s| RefStr { ref_slice: s })
    }
}

impl<'a> RefStrIndex<'a, RefStr> for std::ops::RangeInclusive<usize> {
    type Output = RefStr;

    fn get(self, s: &'a RefStr) -> Option<Self::Output> {
        let inner = s.ref_slice.get(self).map(|s| s);
        inner.map(|s| RefStr { ref_slice: s })
    }
}

impl<'a> RefStrIndex<'a, RefStr> for std::ops::RangeToInclusive<usize> {
    type Output = RefStr;

    fn get(self, s: &'a RefStr) -> Option<Self::Output> {
        let inner = s.ref_slice.get(self).map(|s| s);
        inner.map(|s| RefStr { ref_slice: s })
    }
}

impl RefStr {
    pub fn new(s: &str) -> Self {
        let s = s.as_bytes();
        let s = s.into_iter().copied().collect::<Vec<_>>();
        let s: Arc<[u8]> = Arc::from(s);
        let ref_slice = RefSlice::new(s);
        Self { ref_slice }
    }

    pub fn get<'a, I>(&'a self, index: I) -> Option<I::Output>
    where
        I: RefStrIndex<'a, Self>,
    {
        index.get(self)
    }

    pub fn index<'a, I>(&'a self, index: I) -> I::Output
    where
        I: RefStrIndex<'a, Self>,
    {
        index.get(self).unwrap()
    }

    pub fn len(&self) -> usize {
        self.ref_slice.len()
    }

    pub fn is_empty(&self) -> bool {
        self.ref_slice.is_empty()
    }

    pub fn as_str(&self) -> &str {
        // SAFETY: the RefStr is guaranteed to be a valid UTF-8 string
        unsafe { std::str::from_utf8_unchecked(self.as_ref()) }
    }

    pub fn iter(&self) -> impl Iterator<Item = char> + '_ {
        self.as_str().chars()
    }

    pub fn first(&self) -> Option<char> {
        self.as_str().chars().next()
    }
}

impl From<Arc<str>> for RefStr {
    fn from(s: Arc<str>) -> Self {
        // SAFETY: the Arc<str> is guaranteed to be a valid UTF-8 string
        let s: Arc<[u8]> = unsafe { std::mem::transmute(s) };
        Self {
            ref_slice: RefSlice::new(s),
        }
    }
}

impl From<String> for RefStr {
    fn from(s: String) -> Self {
        let s: Arc<str> = Arc::from(s);
        Self::from(s)
    }
}

impl AsRef<[u8]> for RefStr {
    fn as_ref(&self) -> &[u8] {
        self.ref_slice.as_ref()
    }
}

impl AsRef<str> for RefStr {
    fn as_ref(&self) -> &str {
        // SAFETY: the RefStr is guaranteed to be a valid UTF-8 string
        unsafe { std::str::from_utf8_unchecked(self.as_ref()) }
    }
}
