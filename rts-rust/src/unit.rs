use crate as ddl;
use std::fmt;
use serde::Serialize;

/// A unit type representing an empty struct.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Unit;

impl Unit {
  pub const WIDTH: u32 = 0;
  pub fn to_bits(self) -> ddl::U<0> { 0u8.into() }
  pub fn from_bits_unchecked(_x: ddl::U<0>) -> Self { Unit }
}

ddl::by_value!(Unit);

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{}}")
    }
}

impl fmt::Debug for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{}}")
    }
}

impl Serialize for Unit {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;
        let map = serializer.serialize_map(Some(0))?;
        map.end()
    }
}
