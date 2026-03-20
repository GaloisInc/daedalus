use std::rc::Rc;
use std::marker::PhantomData;

/// A boxed value.
pub struct Val<T: ?Sized> { rc: Rc<T> }

impl<T: ?Sized> Clone for Val<T> {
  fn clone(&self) -> Self { Val { rc: self.rc.clone() } }
}

/// Make new boxed value (reference counted)
pub fn new<T>(x: T) -> Val<T> {
  Val { rc: Rc::new(x) }
}

/// Make a new boxed array (reference counted)
pub fn new_array<const N: usize, T>(x: [T; N]) -> Val<[T]> {
  Val { rc: x.into() }
}

/// Make a new boxed array (reference counted)
pub fn new_array_from_slice<T: Clone>(x:&[T]) -> Val<[T]> {
  Val { rc: x.into() }
}


/// Make a unique value, but try to reuse the first argument, if
/// this is the last reference to it.
pub fn reuse<T>(mut x: Val<T>, y: T) -> Val<T> {
  match Rc::get_mut(&mut (x.rc)) {
    Some(yes) => { *yes = y; x }
    None      => new(y)
  }
}

/// Make a unique value, but tyr to reuse the first argument if possible.
/// Note that we can only reuse the array if the new one happens to be of
/// the same size.
pub fn reuse_array<const N: usize, T>(mut x: Val<[T]>, y: [T; N]) -> Val<[T]> {
  match Rc::get_mut(&mut (x.rc)) {
    Some(yes) => {
      match <&mut [T;N] as TryFrom<&mut [T]>>::try_from(yes) {
        Ok(arr) => { *arr = y; x },
        Err(_)  => new_array(y)
    }}
    _ => new_array(y)
  }
}


/// A borrowed boxed value.
 #[derive(Copy)]
pub struct Ref<'a,T: ?Sized> {
    ptr: *const T,
    lifetime: PhantomData<&'a T>
}

impl<'a, T: ?Sized> Clone for Ref<'a,T> {
  fn clone(&self) -> Self { Ref { ptr: self.ptr, lifetime: self.lifetime } }
}

impl<T: ?Sized> Val<T> {
  /// Borrow a boxed value.
  pub fn borrowed(&self) -> Ref<T> {
    Ref { ptr: Rc::as_ptr(&(self.rc)), lifetime: PhantomData }
  }
}

impl<'a,T: ?Sized> Ref<'a,T> {
  /// Clone a borrowed boxed value.
  pub fn cloned(self) -> Val<T> {
    unsafe {
      Rc::increment_strong_count(self.ptr);
      Val { rc: Rc::from_raw(self.ptr) }
    }
  }
}

impl<T: ?Sized> std::ops::Deref for Val<T> {
  type Target = T;
  fn deref(&self) -> &T { &(self.rc) }
}

impl<'a, T: ?Sized> std::ops::Deref for Ref<'a,T> {
  type Target = T;
  fn deref(&self) -> &T { unsafe { &*self.ptr } }
}