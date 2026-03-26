use crate as ddl;
use std::cmp::Ordering;
use std::rc::Rc;
use std::marker::PhantomData;

/// An owned DDL value.  Uses reference counting.
pub struct O<T: ?Sized> { pub(crate) rc: Rc<T> }

/// Create a new owned value.
pub fn new<T>(x: T) -> O<T> { O { rc: Rc::new(x) } }

/// Create new owned array out of a Rust array.
pub fn new_array<const N: usize, T>(x: [T;N]) -> ddl::Array<T> { O { rc: x.into() } }

/// Create new owned array out of a Rust reference to a slice.
pub fn new_array_slice<T: Clone>(x: &[T]) -> ddl::Array<T> { O { rc: x.into() } }

/// Create new owned array out of a vector.
pub fn new_array_vec<T>(x: Vec<T>) -> ddl::Array<T> { O { rc: x.into() } }

/// Convert a DDL array into a vector.
pub fn array_to_vec<T: Clone>(x: ddl::Array<T>) -> Vec<T> {
  // XXX: It would be nice if this only cloned things when the ref count > 1,
  // but Rc does not seem to expose such functionality.
  let mut res = Vec::with_capacity(x.len());
  res.extend_from_slice(&x.rc);
  res
}

/// Make a unique value, but try to reuse the first argument, if
/// this is the last reference to it.
pub fn reuse<T>(mut x: O<T>, y: T) -> O<T> {
  match Rc::get_mut(&mut x.rc) {
    Some(yes) => { *yes = y; x }
    None      => new(y)
  }
}

/// Make a unique value, but try to reuse the first argument if possible.
/// Note that we can only reuse the array if the new one happens to be of
/// the same size.
pub fn reuse_array<const N: usize, T>(mut x: O<[T]>, y: [T; N]) -> O<[T]> {
  match Rc::get_mut(&mut x.rc) {
    Some(yes) => {
      match <&mut [T;N] as TryFrom<&mut [T]>>::try_from(yes) {
        Ok(arr) => { *arr = y; x },
        Err(_)  => new_array(y)
    }}
    _ => new_array(y)
  }
}

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

impl<T: ?Sized + 'static> ddl::Type for O<T> {
  type B<'a> = B<'a,T>;
  fn borrowed(&self) -> B<T> {
    B { ptr: Rc::as_ptr(&(self.rc)), lifetime: PhantomData }
  }
}

impl<'a,T: ?Sized> ddl::Clonable for B<'a,T> {
  type O = O<T>;
  fn cloned(self) -> Self::O {
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