mod test;
mod ddl_box;
mod bitdata;
mod number;
mod int;
mod input;
mod array;
mod builder;
mod map;
mod map_iterators;
mod maybe;
mod tuple;
mod unit;
mod user_defined;
mod parser_state;
mod exception;

pub use test::*;
pub use ddl_box::*;
pub use number::*;
pub use int::*;
pub use input::*;
pub use map::*;
pub use map_iterators::*;
pub use array::*;
pub use builder::*;
pub use maybe::*;
pub use unit::*;
pub use parser_state::*;
pub use exception::*;

/// DDL types representing owned things that can be borrowed.
/// The main reason for this is because in some cases we'd like to represent
/// borrowed values in a different way than &T;
pub trait Type : Clone + 'static {
  type B<'a> : Clo<O=Self>;
  fn bor(&self) -> Self::B<'_>;
}

/// Types that represent borrowed things that can be cloned to an owned thing.
pub trait Clo {
  type O;
  fn clo(self) -> Self::O;
}

/// Serialization of DDL values using the common DDL JSON schema.
pub trait DDLSerialize {
  fn ddl_serialize<S: serde::Serializer>(&self, serializer: S)
    -> Result<S::Ok, S::Error>;
}

/// Adapt a [`DDLSerialize`] value for use with Serde combinators.
#[derive(Clone, Copy)]
pub struct AsDDL<'a, T: ?Sized>(pub &'a T);

impl<T: ?Sized + DDLSerialize> serde::Serialize for AsDDL<'_, T> {
  fn serialize<S: serde::Serializer>(&self, serializer: S)
    -> Result<S::Ok, S::Error> {
    self.0.ddl_serialize(serializer)
  }
}

impl DDLSerialize for bool {
  fn ddl_serialize<S: serde::Serializer>(&self, serializer: S)
    -> Result<S::Ok, S::Error> {
    serializer.serialize_bool(*self)
  }
}

fn ddl_serialize_float_tag<S: serde::Serializer>(
  tag: &'static str,
  serializer: S,
) -> Result<S::Ok, S::Error> {
  use serde::ser::SerializeMap;
  let mut map = serializer.serialize_map(Some(1))?;
  map.serialize_entry(tag, &())?;
  map.end()
}

impl DDLSerialize for f32 {
  fn ddl_serialize<S: serde::Serializer>(&self, serializer: S)
    -> Result<S::Ok, S::Error> {
    if self.is_infinite() {
      ddl_serialize_float_tag("$$inf", serializer)
    } else if self.is_nan() {
      ddl_serialize_float_tag("$$nan", serializer)
    } else {
      serializer.serialize_f32(*self)
    }
  }
}

impl DDLSerialize for f64 {
  fn ddl_serialize<S: serde::Serializer>(&self, serializer: S)
    -> Result<S::Ok, S::Error> {
    if self.is_infinite() {
      ddl_serialize_float_tag("$$inf", serializer)
    } else if self.is_nan() {
      ddl_serialize_float_tag("$$nan", serializer)
    } else {
      serializer.serialize_f64(*self)
    }
  }
}

/// References are the usual way to represent borrowed values.
impl <T:Clone> Clo for &T {
  type O = T;
  fn clo(self) -> Self::O { self.clone() }
}


/// Helper for simple types that are `Copy` and passed by value.
#[macro_export]
macro_rules! by_value {
  ($ty: ty) => {
    impl $crate::Type for $ty {
      type B<'a> = $ty;
      fn bor(&self) -> $ty { *self }
    }
    impl $crate::Clo for $ty {
      type O = $ty;
      fn clo(self) -> $ty { self }
    }
  };
}

by_value!(());
by_value!(bool);
by_value!(u8);
by_value!(u16);
by_value!(u32);
by_value!(u64);
by_value!(f32);
by_value!(f64);


/// A helper macro for making instances of [Type] for types that
/// are borrowed the usual way (i.e., by reference)
#[macro_export]
macro_rules! by_ref {
  ($nm:ident) => {
    impl Type for $nm {
      type B<'a> = &'a Self;
      fn bor(&self) -> &Self { self }
    }
  };
  ($nm:ident <$($tp: ident),*>) => {
    impl<$($tp: Type),*> Type for $nm<$($tp),*> {
      type B<'a> = &'a Self;
      fn bor(&self) -> &Self { self }
    }
  };
}

by_ref!(Option<T>);
