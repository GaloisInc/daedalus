use crate as ddl;
use ddl::Clo;
use std::fmt as fmt;

/// Owned array
#[repr(transparent)]
#[derive(PartialEq,Eq,PartialOrd,Ord)]
pub struct Array<T> { rc: ddl::O<[T]> }

/// Borrowed array
#[repr(transparent)]
#[derive(Copy,PartialEq,Eq,PartialOrd,Ord)]
pub struct ArrayB<'a,T> { rc: ddl::B<'a,[T]> }

impl<T: ddl::Type> ddl::Type for Array<T> {
  type B<'a> = ArrayB<'a,T>;
  fn bor(&self) -> ArrayB<T> { ArrayB { rc: self.rc.bor() } }
}

impl <T: ddl::Type> ddl::Clo for ArrayB<'_,T> {
  type O = Array<T>;
  fn clo(self) -> Array<T> { Array { rc: self.rc.clo() } }
}

impl<T> Clone for Array<T> {
  fn clone(&self) -> Self { Array { rc: self.rc.clone() } }
}

impl<'a,T> Clone for ArrayB<'a,T> {
  fn clone(&self) -> Self { ArrayB { rc: self.rc.clone() } }
}

impl<T> std::ops::Deref for Array<T> {
  type Target = [T];
  fn deref(&self) -> &[T] { &self.rc }
}

impl<'a,T> std::ops::Deref for ArrayB<'a,T> {
  type Target = [T];
  fn deref(&self) -> &[T] { &self.rc }
}




/// Create new owned array out of a Rust array.
pub fn new_array<const N: usize, T>(x: [T;N]) -> ddl::Array<T> {
  Array { rc: ddl::O { rc: x.into() } }
}

/// Create a new byte array out of a reference to some bytes.
pub fn new_byte_array(x: &[u8]) -> ddl::Array<ddl::U<8>> {
  Array { rc: ddl::O { rc: x.into_iter().copied().map(|a| a.into()).collect() } }
}

/// Create new owned array out of a Rust reference to a slice.
pub fn new_array_slice<T: Clone>(x: &[T]) -> ddl::Array<T> {
  Array { rc: ddl::O { rc: x.into() } }
}

/// Create new owned array out of a vector.
pub fn new_array_vec<T>(x: Vec<T>) -> ddl::Array<T> {
  Array { rc: ddl::O { rc: x.into() } }
}

/// Convert a DDL array into a vector.
pub fn array_to_vec<T: Clone>(x: ddl::Array<T>) -> Vec<T> {
  // XXX: It would be nice if this only cloned things when the ref count > 1,
  // But we'd have to do this unsafely
  x.into_iter().cloned().collect()
}

/// Convert a DDL array into a vector of bytes.
pub fn array_to_byte_vec<T: Clone>(x: ddl::Array<ddl::U<8>>) -> Vec<u8> {
  // XXX: It would be nice if this only cloned things when the ref count > 1,
  // But we'd have to do this unsafely
  x.into_iter().cloned().map(|x|x.into()).collect()
}



impl<'a,T: ddl::Type> ArrayB<'a,Array<T>> {
  pub fn concat(self) -> Array<T> {
    let mut b = ddl::new_builder();
    for i in 0 .. self.len() {
      b = b.push_array(self[i].clo());
    }
    b.build()
  }
}

/// A helper type for iterating over array (owned from)
#[derive(Clone)]
pub struct ArrayIterator<T> {
  index: usize,
  array: Array<T>
}

/// A helper type for iterating over array (borrowed from)
#[derive(Clone,Copy)]
pub struct ArrayIteratorB<'a,T> {
  index: usize,
  array: ArrayB<'a,T>
}

impl<T: ddl::Type> ddl::Type for ArrayIterator<T> {
  type B<'a> = ArrayIteratorB<'a,T>;
  fn bor(&self) -> ArrayIteratorB<T> {
    ArrayIteratorB { index: self.index, array: self.array.bor() }
  }
}

impl<'a, T: ddl::Type> Clo for ArrayIteratorB<'a,T> {
  type O = ArrayIterator<T>;
  fn clo(self) -> ArrayIterator<T> {
    ArrayIterator { index: self.index, array: self.array.clo() }
  }
}

pub fn new_array_iterator<T>(xs: Array<T>) -> ArrayIterator<T> {
  ArrayIterator { index: 0, array: xs }
}

impl<'a,T: ddl::Type> ArrayIteratorB<'a,T> {
  pub fn ddl_done(self) -> bool { self.index >= self.array.len() }
  pub fn ddl_key(self) -> usize { self.index }
  pub fn ddl_val(self) -> T     { self.array[self.index].clo() }
}

impl <T> ArrayIterator<T> {
  pub fn ddl_next(self) -> ArrayIterator<T> {
    ArrayIterator { index: self.index + 1, array: self.array }
  }
}

fn fmt_array<T, F>(slice: &[T], f: &mut fmt::Formatter<'_>, fmt_item: F) -> fmt::Result
where
  F: Fn(&T, &mut fmt::Formatter<'_>) -> fmt::Result,
{
  write!(f, "[")?;
  for (i, item) in slice.iter().enumerate() {
    if i > 0 {
      write!(f, ", ")?;
    }
    fmt_item(item, f)?;
  }
  write!(f, "]")
}

impl<T: fmt::Display> fmt::Display for Array<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt_array(&**self, f, |item, f| write!(f, "{}", item))
  }
}

impl<T: fmt::Debug> fmt::Debug for Array<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt_array(&**self, f, |item, f| write!(f, "{:?}", item))
  }
}

impl<'a, T: fmt::Display> fmt::Display for ArrayB<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt_array(&**self, f, |item, f| write!(f, "{}", item))
  }
}

impl<'a, T: fmt::Debug> fmt::Debug for ArrayB<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt_array(&**self, f, |item, f| write!(f, "{:?}", item))
  }
}

