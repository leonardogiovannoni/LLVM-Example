// custom implementation of rc without weak references

use std::{cell::Cell, ops::Deref, ptr::NonNull};

#[repr(C)]
struct RcBox<T: ?Sized> {
    ref_count: Cell<usize>,
    value: T,
}

#[repr(transparent)]
pub struct Rc<T: ?Sized> {
    ptr: NonNull<RcBox<T>>,
}

impl<T> Rc<T> {
    pub fn new(value: T) -> Self {
        let ptr = Box::into_raw(Box::new(RcBox {
            ref_count: Cell::new(1),
            value,
        }));

        // SAFETY: Box::into_raw returns a non-null pointer
        Self {
            ptr: unsafe { NonNull::new_unchecked(ptr) },
        }
    }
}

impl<T: ?Sized> Clone for Rc<T> {
    fn clone(&self) -> Self {
        // SAFETY: self.ptr is a valid pointer
        let this = unsafe { &(*self.ptr.as_ptr()) };
        this.ref_count.set(this.ref_count.get() + 1);
        Self { ptr: self.ptr }
    }
}

impl<T: ?Sized> Drop for Rc<T> {
    fn drop(&mut self) {
        let this = unsafe { &(*self.ptr.as_ptr()) };
        // SAFETY if the ref_count is 1, then we are the last reference to this data
        if this.ref_count.get() == 1 {
            // so we need to deallocate the data
            let b = unsafe { Box::from_raw(self.ptr.as_ptr()) };
            drop(b);
        } else {
            // otherwise, we need to decrement the ref_count
            this.ref_count.set(this.ref_count.get() - 1);
        }
    }
}

impl<T: ?Sized> Deref for Rc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        // SAFETY: self.ptr is a valid pointer
        unsafe { &self.ptr.as_ref().value }
    }
}
