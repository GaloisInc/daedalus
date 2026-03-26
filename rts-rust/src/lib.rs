mod ddl_box;
mod number;
mod input;
mod builder;

pub use ddl_box::*;
pub use number::*;
pub use input::*;
pub use builder::*;

/// DDL types representing owned things that can be borrowed.
/// The main reason for this is because in some cases we'd like to represent
/// borrowed values in a different way than &T;
pub trait Type : Clone + 'static {
  type B<'a> : Clonable<O=Self>;
  fn borrowed(&self) -> Self::B<'_>;
}

/// Types that represent borrowed things that can be cloned to an owned thing.
pub trait Clonable {
  type O;
  fn cloned(self) -> Self::O;
}

/// References are the usual way to represent borrowed values.
impl <T:Clone> Clonable for &T {
  type O = T;
  fn cloned(self) -> Self::O { self.clone() }
}

/// Owned array
pub type Array<T>       = O<[T]>;

/// Borrowed array
pub type ArrayB<'a,T>   = B<'a,[T]>;


/// Helper for simple types that are `Copy` and passed by value.
#[macro_export]
macro_rules! by_value {
  ($ty: ty) => {
    impl $crate::Type for $ty {
      type B<'a> = $ty;
      fn borrowed(&self) -> Self::B<'_> { *self }
    }
    impl $crate::Clonable for $ty {
      type O = $ty;
      fn cloned(self) -> Self::O { self }
    }
  };
}

by_value!(());
by_value!(bool);
by_value!(u8);
by_value!(u16);
by_value!(u32);
by_value!(u64);


/// A helper macro for making instances of [Type] for types that
/// are borrowed the usual way (i.e., by reference)
#[macro_export]
macro_rules! by_ref {
  ($nm:ident) => {
    impl Type for $nm {
      type B<'a> = &'a Self;
      fn borrowed(&self) -> Self::B<'_> { self }
    }
  };
  ($nm:ident <$($tp: ident),*>) => {
    impl<$($tp: Type),*> Type for $nm<$($tp),*> {
      type B<'a> = &'a Self;
      fn borrowed(&self) -> Self::B<'_> { self }
    }
  };
}

by_ref!(Option<T>);



