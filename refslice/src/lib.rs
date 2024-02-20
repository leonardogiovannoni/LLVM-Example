
use triomphe::Arc;

pub mod refstr;
use std::{
    fmt::Debug, ops::{Index, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive}, rc::Rc
};

use std::hash::{Hash, Hasher};

#[derive(Clone)]
pub struct RefSlice<T> {
    inner: Arc<[T]>,
    start: usize,
    end: usize,
}


// Added the lifetime 'a to the trait to indicate the lifetime of references returned by get.
pub trait RefSliceIndex<'a, S: ?Sized + 'a> {
    // Here, `S: 'a` indicates that `S` lives at least as long as 'a.
    type Output: 'a;
    fn get(&self, slice: &'a S) -> Option<Self::Output>;
    fn is_in_bounds(&self, slice: &S) -> bool;
}

// Implement the trait for usize, where 'a denotes the lifetime of the returned reference.
impl<'a, S: 'a> RefSliceIndex<'a, RefSlice<S>> for usize {
    type Output = &'a S;

    fn get(&self, slice: &'a RefSlice<S>) -> Option<Self::Output> {
        if self.is_in_bounds(slice) {
            Some(&(*slice.inner)[slice.start + *self])
        } else {
            None
        }
    }

    fn is_in_bounds(&self, slice: &RefSlice<S>) -> bool {
        *self < slice.wrapper_len()
    }
}

impl<'a, S: 'a> RefSliceIndex<'a, RefSlice<S>> for Range<usize> {
    type Output = RefSlice<S>;

    fn get(&self, slice: &'a RefSlice<S>) -> Option<Self::Output> {
        if self.is_in_bounds(slice) {
            Some(RefSlice {
                inner: Arc::clone(&slice.inner),
                start: slice.start + self.start,
                end: slice.start + self.end,
            })
        } else {
            if self.start == self.end && self.end == slice.wrapper_len() {
                Some(RefSlice {
                    inner: Arc::clone(&slice.inner),
                    start: slice.start + self.start,
                    end: slice.start + self.end,
                })
            } else {
                None
            }
        }
    }

    fn is_in_bounds(&self, slice: &RefSlice<S>) -> bool {
        self.start < self.end && self.start < slice.wrapper_len() && self.end <= slice.wrapper_len()
    }
}

impl<'a, S: 'a> RefSliceIndex<'a, RefSlice<S>> for RangeTo<usize> {
    type Output = RefSlice<S>;

    fn get(&self, slice: &'a RefSlice<S>) -> Option<Self::Output> {
        if self.is_in_bounds(slice) {
            Some(RefSlice {
                inner: Arc::clone(&slice.inner),
                start: slice.start,
                end: slice.start + self.end,
            })
        } else {
            None
        }
    }

    fn is_in_bounds(&self, slice: &RefSlice<S>) -> bool {
        self.end <= slice.wrapper_len()
    }
}


impl<'a, S: 'a> RefSliceIndex<'a, RefSlice<S>> for RangeToInclusive<usize> {
    type Output = RefSlice<S>;

    fn get(&self, slice: &'a RefSlice<S>) -> Option<Self::Output> {
        if self.is_in_bounds(slice) {
            Some(RefSlice {
                inner: Arc::clone(&slice.inner),
                start: slice.start,
                end: slice.start + self.end + 1,
            })
        } else {
            if self.end + 1 == slice.wrapper_len() {
                Some(RefSlice {
                    inner: Arc::clone(&slice.inner),
                    start: slice.start,
                    end: slice.start + self.end + 1,
                })
            } else {
                None
            } 
        }
    }

    fn is_in_bounds(&self, slice: &RefSlice<S>) -> bool {
        self.end < slice.wrapper_len()
    }
}

impl<'a, S: 'a> RefSliceIndex<'a, RefSlice<S>> for RangeInclusive<usize> {
    type Output = RefSlice<S>;

    fn get(&self, slice: &'a RefSlice<S>) -> Option<Self::Output> {
        if self.is_in_bounds(slice) {
            Some(RefSlice {
                inner: Arc::clone(&slice.inner),
                start: slice.start + *self.start(),
                end: slice.start + *self.end() + 1,
            })
        } else {
            if self.start() + 1 == slice.wrapper_len() {
                Some(RefSlice {
                    inner: Arc::clone(&slice.inner),
                    start: slice.start + *self.start(),
                    end: slice.start + *self.end() + 1,
                })
            } else {
                None
            }
        }
    }

    fn is_in_bounds(&self, slice: &RefSlice<S>) -> bool {
        *self.start() < slice.wrapper_len() && *self.end() < slice.wrapper_len()
    }

}

impl<'a, S: 'a> RefSliceIndex<'a, RefSlice<S>> for RangeFrom<usize> {
    type Output = RefSlice<S>;

    fn get(&self, slice: &'a RefSlice<S>) -> Option<Self::Output> {
        if self.is_in_bounds(slice) {
            Some(RefSlice {
                inner: Arc::clone(&slice.inner),
                start: slice.start + self.start,
                end: slice.end,
            })
        } else {
            if self.start == slice.wrapper_len() {
                Some(RefSlice {
                    inner: Arc::clone(&slice.inner),
                    start: slice.start + self.start,
                    end: slice.end,
                })
            } else {
                None
            }
        }
    }

    fn is_in_bounds(&self, slice: &RefSlice<S>) -> bool {
        self.start < slice.wrapper_len()
    }
}

impl<'a, S: 'a> RefSliceIndex<'a, RefSlice<S>> for RangeFull {
    type Output = RefSlice<S>;

    fn get(&self, slice: &'a RefSlice<S>) -> Option<Self::Output> {
        Some(RefSlice {
            inner: Arc::clone(&slice.inner),
            start: slice.start,
            end: slice.end,
        })
    }

    fn is_in_bounds(&self, _slice: &RefSlice<S>) -> bool {
        true
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for RefSlice<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.as_ref().iter()).finish()
    }
}




impl<T> From<Arc<[T]>> for RefSlice<T> {
    fn from(inner: Arc<[T]>) -> Self {
        let len = inner.len();
        RefSlice {
            inner,
            start: 0,
            end: len,
        }
    }
}

impl<T, const N: usize> From<[T; N]> for RefSlice<T> {
    fn from(inner: [T; N]) -> Self {
        let inner = inner.into_iter().collect::<Vec<_>>();
        let slice: Arc<[T]> = Arc::from(inner);
        From::from(slice)
    }
}



impl<T> RefSlice<T> {
    fn range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn new(inner: Arc<[T]>) -> Self {
        From::from(inner)
    }

    pub fn len(&self) -> usize {
        self.wrapper_len()
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.as_ref().iter()
    }


    fn wrapper_len(&self) -> usize {
        self.range().len()
    }

    pub fn is_empty(&self) -> bool {
        self.wrapper_len() == 0
    }
    pub fn get<'a, I>(&'a self, index: I) -> Option<I::Output>
    where
        I: RefSliceIndex<'a, Self>,
    {
        index.get(self)
    }

    pub fn index<'a, I: Debug>(&'a self, index: I) -> I::Output
    where
        I: RefSliceIndex<'a, Self>,
    {        
        index.get(self).unwrap()
    }

    pub fn first(&self) -> Option<&T> {
        self.get(0)
    }

    pub fn last(&self) -> Option<&T> {
        self.wrapper_len().checked_sub(1).and_then(|i| self.get(i))
    }
}

impl<T> AsRef<[T]> for RefSlice<T> {
    fn as_ref(&self) -> &[T] {
        &self.inner[self.range()]
    }
}

impl<T> Index<usize> for RefSlice<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}


impl<T: PartialEq> PartialEq for RefSlice<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: Eq> Eq for RefSlice<T> {}


impl<T: Hash> Hash for RefSlice<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let a = Arc::new([1, 2, 3, 4, 5]);
        let b = RefSlice::new(a.clone());
        let c = b.get(1);
        let d = b.get(1..3);
        let empty = b.get(5..5);
        assert_eq!(c, Some(&2));
        assert_eq!(d, Some(RefSlice::new(Arc::new([2, 3]))));
        assert_eq!(empty, Some(RefSlice::new(Arc::new([]))));
        let tmp = b.get(..4);
        assert_eq!(tmp, Some(RefSlice::new(Arc::new([1, 2, 3, 4]))));
    }

    #[test]
    fn access_out_of_bounds() {
        let a = Arc::new([1, 2, 3, 4, 5]);
        let slice = RefSlice::new(a);

        assert_eq!(slice.get(5).as_ref(), slice.as_ref().get(5).as_ref());
        assert_eq!(slice.get(0..100).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..100).map(|x| x.to_owned()));
        assert_eq!(slice.get(0..6).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..6).map(|x| x.to_owned()));
        assert_eq!(slice.get(4..=4).map(|x| x.as_ref().to_owned()), slice.as_ref().get(4..=4).map(|x| x.to_owned()));
        assert_eq!(slice.get(4..5).map(|x| x.as_ref().to_owned()), slice.as_ref().get(4..5).map(|x| x.to_owned()));
        assert_eq!(slice.get(5..5).map(|x| x.as_ref().to_owned()), slice.as_ref().get(5..5).map(|x| x.to_owned()));
        assert_eq!(slice.get(6..6).map(|x| x.as_ref().to_owned()), slice.as_ref().get(6..6).map(|x| x.to_owned()));
        assert_eq!(slice.get(..6).map(|x| x.as_ref().to_owned()), slice.as_ref().get(..6).map(|x| x.to_owned()));
        assert_eq!(slice.get(..=5).map(|x| x.as_ref().to_owned()), slice.as_ref().get(..=5).map(|x| x.to_owned()));
    }

    #[test]
    fn access_with_empty_slice() {
        let a: Arc<[i32]> = Arc::new([]);
        let slice = RefSlice::new(a);

        assert_eq!(slice.get(0), None);
        assert_eq!(slice.get(0..0).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..0).map(|x| x.to_owned()));
        assert_eq!(slice.get(1..1).map(|x| x.as_ref().to_owned()), slice.as_ref().get(1..1).map(|x| x.to_owned()));
        assert_eq!(slice.get(..).map(|x| x.as_ref().to_owned()), slice.as_ref().get(..).map(|x| x.to_owned()));
        assert_eq!(slice.get(0..=0).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..=0).map(|x| x.to_owned()));
        assert_eq!(slice.get(1..=1).map(|x| x.as_ref().to_owned()), slice.as_ref().get(1..=1).map(|x| x.to_owned()));
        assert_eq!(slice.get(..=0).map(|x| x.as_ref().to_owned()), slice.as_ref().get(..=0).map(|x| x.to_owned()));
        assert_eq!(slice.get(..=1).map(|x| x.as_ref().to_owned()), slice.as_ref().get(..=1).map(|x| x.to_owned()));
        assert_eq!(slice.get(..0).map(|x| x.as_ref().to_owned()), slice.as_ref().get(..0).map(|x| x.to_owned()));
        assert_eq!(slice.get(1..).map(|x| x.as_ref().to_owned()), slice.as_ref().get(1..).map(|x| x.to_owned()));
        assert_eq!(slice.get(0..).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..).map(|x| x.to_owned()));
        assert_eq!(slice.get(0..=0).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..=0).map(|x| x.to_owned()));
        assert_eq!(slice.get(1..=1).map(|x| x.as_ref().to_owned()), slice.as_ref().get(1..=1).map(|x| x.to_owned()));
        assert_eq!(slice.get(0..=0).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..=0).map(|x| x.to_owned()));
        assert_eq!(slice.get(1..=1).map(|x| x.as_ref().to_owned()), slice.as_ref().get(1..=1).map(|x| x.to_owned()));
        assert_eq!(slice.get(0..=0).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..=0).map(|x| x.to_owned()));
        assert_eq!(slice.get(1..=1).map(|x| x.as_ref().to_owned()), slice.as_ref().get(1..=1).map(|x| x.to_owned()));

    }

    #[test]
    fn inclusive_range_edge_case() {
        let a = Arc::new([1, 2, 3, 4, 5]);
        let slice = RefSlice::new(a);
        assert_eq!(slice.get(0..=4).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..=4).map(|x| x.to_owned()));
        assert_eq!(slice.get(4..=4).map(|x| x.as_ref().to_owned()), slice.as_ref().get(4..=4).map(|x| x.to_owned()));
        assert_eq!(slice.get(5..=5).map(|x| x.as_ref().to_owned()), slice.as_ref().get(5..=5).map(|x| x.to_owned()));
        assert_eq!(slice.get(6..=6).map(|x| x.as_ref().to_owned()), slice.as_ref().get(6..=6).map(|x| x.to_owned()));
        assert_eq!(slice.get(..=5).map(|x| x.as_ref().to_owned()), slice.as_ref().get(..=5).map(|x| x.to_owned()));
        assert_eq!(slice.get(..=4).map(|x| x.as_ref().to_owned()), slice.as_ref().get(..=4).map(|x| x.to_owned()));
        assert_eq!(slice.get(..=6).map(|x| x.as_ref().to_owned()), slice.as_ref().get(..=6).map(|x| x.to_owned()));
        assert_eq!(slice.get(0..=0).map(|x| x.as_ref().to_owned()), slice.as_ref().get(0..=0).map(|x| x.to_owned()));
        assert_eq!(slice.get(1..=1).map(|x| x.as_ref().to_owned()), slice.as_ref().get(1..=1).map(|x| x.to_owned()));
    }

    #[test]
    fn range_from_edge_case() {
        let a = Arc::new([1, 2, 3, 4, 5]);
        let slice = RefSlice::new(a);
        assert_eq!(slice.get(5..).map(|x| x.as_ref().to_owned()), slice.as_ref().get(5..).map(|x| x.to_owned()));
        assert_eq!(slice.get(4..).map(|x| x.as_ref().to_owned()), slice.as_ref().get(4..).map(|x| x.to_owned()));
        assert_eq!(slice.get(6..).map(|x| x.as_ref().to_owned()), slice.as_ref().get(6..).map(|x| x.to_owned()));
    }

    #[test]
    fn range_to_edge_case() {
        let a = Arc::new([1, 2, 3, 4, 5]);
        let slice = RefSlice::new(a);

        assert_eq!(slice.get(..5), Some(RefSlice::new(Arc::new([1, 2, 3, 4, 5]))));
        assert_eq!(slice.get(..6), None);
        assert_eq!(slice.get(..0), Some(RefSlice::new(Arc::new([]))));
    }
}



