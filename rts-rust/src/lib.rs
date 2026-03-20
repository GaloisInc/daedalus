mod boxed;
mod number;
mod input;

pub use boxed::*;
pub use number::*;
pub use input::*;

type Array<T> = Val<[T]>;
type ArrayRef<'a,T> = Ref<'a,[T]>;
