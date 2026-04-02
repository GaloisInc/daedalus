use crate as ddl;
use std::fmt;
use serde::Serialize;

/// A Maybe type, similar to Haskell's Maybe.
/// Either contains a value (Just) or is empty (Nothing).
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Maybe<T> {
    Nothing,
    Just(T),
}

impl<T> Maybe<T> {
    /// Unwraps the Maybe, panicking if it is Nothing.
    pub fn unwrap(self) -> T {
        match self {
            Maybe::Just(x) => x,
            Maybe::Nothing => panic!("called `Maybe::unwrap()` on a `Nothing` value"),
        }
    }
}

impl<T: ddl::Type> ddl::Type for Maybe<T> {
    type B<'a> = Maybe<T::B<'a>>;
    fn bor(&self) -> Maybe<T::B<'_>> {
        match self {
            Maybe::Nothing => Maybe::Nothing,
            Maybe::Just(x) => Maybe::Just(x.bor()),
        }
    }
}

impl<'a, T: ddl::Clo> ddl::Clo for Maybe<T> {
    type O = Maybe<T::O>;
    fn clo(self) -> Self::O {
        match self {
            Maybe::Nothing => Maybe::Nothing,
            Maybe::Just(x) => Maybe::Just(x.clo()),
        }
    }
}

impl<T: fmt::Display> fmt::Display for Maybe<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Maybe::Nothing => write!(f, "nothing"),
            Maybe::Just(x) => write!(f, "just {}", x),
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Maybe<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Maybe::Nothing => write!(f, "nothing"),
            Maybe::Just(x) => write!(f, "just {:?}", x),
        }
    }
}

impl<T: Serialize> Serialize for Maybe<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;
        match self {
            Maybe::Nothing => serializer.serialize_none(),
            Maybe::Just(x) => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("$$just", x)?;
                map.end()
            }
        }
    }
}
