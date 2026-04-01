use crate as ddl;
use ddl::Clo;

/// Owned array
pub type Array<T>       = ddl::O<[T]>;

/// Borrowed array
pub type ArrayB<'a,T>   = ddl::B<'a,[T]>;


/// Create new owned array out of a Rust array.
pub fn new_array<const N: usize, T>(x: [T;N]) -> ddl::Array<T> { ddl::O { rc: x.into() } }

/// Create a new byte array out of a reference to some bytes.
pub fn new_byte_array(x: &[u8]) -> ddl::Array<ddl::U<8>> {
  ddl::O { rc: x.into_iter().copied().map(|a| a.into()).collect() }
}

/// Create new owned array out of a Rust reference to a slice.
pub fn new_array_slice<T: Clone>(x: &[T]) -> ddl::Array<T> { ddl::O { rc: x.into() } }

/// Create new owned array out of a vector.
pub fn new_array_vec<T>(x: Vec<T>) -> ddl::Array<T> { ddl::O { rc: x.into() } }

/// Convert a DDL array into a vector.
pub fn array_to_vec<T: Clone>(x: ddl::Array<T>) -> Vec<T> {
  // XXX: It would be nice if this only cloned things when the ref count > 1,
  // but Rc does not seem to expose such functionality.
  let mut res = Vec::with_capacity(x.len());
  res.extend_from_slice(&x.rc);
  res
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