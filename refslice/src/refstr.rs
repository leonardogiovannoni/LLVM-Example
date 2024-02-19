use std::rc::Rc;
use crate::RefSlice;


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
        let s: Rc<[u8]> = Rc::from(s.as_bytes().to_owned().into_boxed_slice());
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
        unsafe {
            // SAFETY: the RefStr is guaranteed to be a valid UTF-8 string
            std::str::from_utf8_unchecked(self.as_ref())
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = char> + '_ {
        self.as_str().chars()
    }

    pub fn first(&self) -> Option<char> {
        self.as_str().chars().next()
    }


}

impl From<Rc<str>> for RefStr {
    fn from(s: Rc<str>) -> Self {
        // convert the Rc<str> to a Rc<[u8]>
        let s: Rc<[u8]> = unsafe {
            // SAFETY: the Rc<str> is guaranteed to be a valid UTF-8 string
            std::mem::transmute(s)
        };
        Self { ref_slice: RefSlice::new(s) }
    }
}

impl AsRef<[u8]> for RefStr {
    fn as_ref(&self) -> &[u8] {
        self.ref_slice.as_ref()
    }
}

impl AsRef<str> for RefStr {
    fn as_ref(&self) -> &str {
        unsafe {
            // SAFETY: the RefStr is guaranteed to be a valid UTF-8 string
            std::str::from_utf8_unchecked(self.as_ref())
        }
    }
}

