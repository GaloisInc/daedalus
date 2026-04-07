use crate as ddl;
use std::cmp::Ordering;
use std::rc::Rc;
use std::marker::PhantomData;
use std::fmt;
use serde::Serialize;

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

/// A unique owned reference
#[repr(transparent)]
pub struct Uniq<T: ?Sized> { rc: Rc<T> }

pub fn new_uniq<T>(x: T) -> Uniq<T> { Uniq { rc: Rc::new(x) } }

impl<T: ?Sized> Uniq<T> {
  pub fn to_o(self) -> O<T> { O { rc: self.rc } }
  pub fn to_mut(&mut self) -> &mut T {
    let p: *mut T = Rc::as_ptr(&self.rc) as *mut T;
    unsafe { &mut *p }
  }
}

impl<T: ?Sized + Clone> O<T> {
  pub fn to_uniq(mut self) -> Uniq<T> {
    Rc::make_mut(&mut self.rc);
    Uniq { rc: self.rc }
  }
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

impl<'a, T: ?Sized> std::ops::Deref for Uniq<T> {
  type Target = T;
  fn deref(&self) -> &T { &(self.rc) }
}

impl<'a, T: ?Sized> B<'a, T> {
  /// Get a reference with the full 'a lifetime
  pub fn as_ref(&self) -> &'a T {
    unsafe { &*self.ptr }
  }
}

impl<'a, T: ?Sized> std::ops::Deref for B<'a,T> {
  type Target = T;
  fn deref(&self) -> &T { self.as_ref() }
}

impl<T: ?Sized + fmt::Display> fmt::Display for O<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(&**self, f)
  }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for O<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Debug::fmt(&**self, f)
  }
}

impl<'a, T: ?Sized + fmt::Display> fmt::Display for B<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(&**self, f)
  }
}

impl<'a, T: ?Sized + fmt::Debug> fmt::Debug for B<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Debug::fmt(&**self, f)
  }
}

impl<T: ?Sized + Serialize> Serialize for O<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    (&**self).serialize(serializer)
  }
}

impl<'a, T: ?Sized + Serialize> Serialize for B<'a, T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    (&**self).serialize(serializer)
  }
}