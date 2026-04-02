use crate as ddl;
use std::cmp::Ordering;
use std::rc::Rc;
use std::marker::PhantomData;

/// An owned DDL value.  Uses reference counting.
#[repr(transparent)]
pub struct O<T: ?Sized> { pub(crate) rc: Rc<T> }

/// Create a new owned value.
pub fn new<T>(x: T) -> O<T> { O { rc: Rc::new(x) } }

impl <'a, T: ?Sized> Clone for O<T> {
  fn clone(&self) -> O<T> { O { rc: self.rc.clone() } }
}

impl <T: ?Sized + PartialEq> PartialEq for O<T> {
  fn eq(&self, other: &Self) -> bool { self.rc == other.rc }
}

impl <T: ?Sized + Eq> Eq for O<T> {}

impl <T: ?Sized + PartialOrd> PartialOrd for O<T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> { self.rc.partial_cmp(&other.rc) }
}

impl <T: ?Sized + Ord> Ord for O<T> {
  fn cmp(&self, other: &Self) -> Ordering { self.rc.cmp(&other.rc) }
}


/// A borrowed DDL value.
pub struct B<'a,T: ?Sized> {
    ptr: *const T,
    lifetime: PhantomData<&'a T>
}

impl <'a,T: ?Sized> Copy for B<'a,T> {}

impl<'a, T: ?Sized> Clone for B<'a,T> {
  fn clone(&self) -> Self { B { ptr: self.ptr, lifetime: self.lifetime } }
}

impl <'a,T: ?Sized + PartialEq> PartialEq for B<'a,T> {
  fn eq(&self, other: &Self) -> bool { &self == &other }
}

impl <'a,T: ?Sized + Eq> Eq for B<'a,T> {}

impl <'a,T: ?Sized + PartialOrd> PartialOrd for B<'a,T> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    (&self).partial_cmp(&other)
  }
}

impl <'a,T: ?Sized + Ord> Ord for B<'a,T> {
  fn cmp(&self, other: &Self) -> Ordering {
    (&self).cmp(&other)
  }
}


impl<T: ?Sized + 'static> ddl::Type for O<T> {
  type B<'a> = B<'a,T>;
  fn bor(&self) -> B<T> {
    B { ptr: Rc::as_ptr(&(self.rc)), lifetime: PhantomData }
  }
}

impl<'a,T: ?Sized> ddl::Clo for B<'a,T> {
  type O = O<T>;
  fn clo(self) -> Self::O {
    unsafe {
      Rc::increment_strong_count(self.ptr);
      O { rc: Rc::from_raw(self.ptr) }
    }
  }
}

impl<'a, T: ?Sized> std::ops::Deref for O<T> {
  type Target = T;
  fn deref(&self) -> &T { &(self.rc) }
}

impl<'a, T: ?Sized> std::ops::Deref for B<'a,T> {
  type Target = T;
  fn deref(&self) -> &T { unsafe { &*self.ptr } }
}