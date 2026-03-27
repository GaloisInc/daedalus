use crate as ddl;
use ddl::Clo;

/// Owned array
pub type Array<T>       = ddl::O<[T]>;

/// Borrowed array
pub type ArrayB<'a,T>   = ddl::B<'a,[T]>;

impl<'a,T: ddl::Type> ArrayB<'a,Array<T>> {
  pub fn concat(self) -> Array<T> {
    let mut b = ddl::new_builder();
    for i in 0 .. self.len() {
      b = b.push_array(self[i].clo());
    }
    b.build()
  }
}

#[derive(Clone)]
pub struct ArrayIterator<T> {
  index: usize,
  array: Array<T>
}

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