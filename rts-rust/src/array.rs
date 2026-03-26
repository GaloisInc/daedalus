use crate as ddl;
use ddl::Clo;

/// Owned array
pub type Array<T>       = ddl::O<[T]>;

/// Borrowed array
pub type ArrayB<'a,T>   = ddl::B<'a,[T]>;

pub fn concat<T: ddl::Type> (xs: ArrayB<Array<T>>) -> Array<T> {
  let mut b = ddl::new_builder();
  for i in 0 .. xs.len() {
    b = b.push_array(xs[i].clo());
  }
  b.build()
}