use std::rc::Rc;
use std::marker::PhantomData;
 
 #[derive(Copy,Clone)]

/// A borrowed RC.  This differs from `&RC` in that we avoid an additional
/// indirection.
pub struct BRc<'a,T> {
    ptr: *const T,
    lifetime: PhantomData<&'a T>
}

pub fn borrow_rc<'a,T>(x: &'a Rc<T>) -> BRc<'a,T> {
  BRc { ptr: Rc::as_ptr(x), lifetime: PhantomData }
}

pub fn clone_borrowed_rc<T>(x: &BRc<T>) -> Rc<T> {
  unsafe {
    Rc::increment_strong_count(x.ptr);
    Rc::from_raw(x.ptr)
  }
}

impl<'a, T: Sized> std::ops::Deref for BRc<'a,T> {
  type Target = T;
  fn deref(&self) -> &T {
    unsafe { &*self.ptr }
  }
}