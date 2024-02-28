use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use fxhash::FxHashMap as HashMap;

#[derive(Debug)]
pub struct Arena<K, T> {
    // TODO: up to now, we could use UnsafeCell instead of RefCell
    data: RefCell<HashMap<u32, Rc<T>>>,
    _marker: std::marker::PhantomData<K>,
    next_id: Cell<u32>,
}

impl<K, T> Default for Arena<K, T> {
    fn default() -> Self {
        Self {
            data: Default::default(),
            _marker: Default::default(),
            next_id: Cell::new(1),
        }
    }
}

impl<K: Into<u32> + From<u32>, T> Arena<K, T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&self, value: T) -> K {
        let id = self.next_id.get();
        self.next_id.set(id + 1);

        self.data.borrow_mut().insert(id, Rc::new(value));
        K::from(id)
        //let b = Box::into_raw(Box::new(value));
        //b as usize
    }

    #[inline(always)]
    pub fn get(&self, id: K) -> Option<impl std::ops::Deref<Target = T> + '_> {
        let id = id.into();
        self.data.borrow().get(&id).cloned()
        //unsafe {
        //    (*self.data.get()).get(&id).cloned()
        //}
        //let b = id as *const T;
        //if b.is_null() {
        //    None
        //} else {
        //    Some(unsafe { &*b })
        //}
    }
}
