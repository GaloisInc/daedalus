use std::ops as ops;
use std::fmt as fmt;
use crate as ddl;
use serde::Serialize;

/// Operations that should be supported by representation types for [Word].
/// Checked arithmetic (op_add, op_sub, op_mul) returns (result, overflow_flag).
/// Generated code uses these to detect overflow and throw an exception.
pub trait Ops : Copy + PartialEq + Eq + PartialOrd + Ord {
  const WIDTH: u32;

  fn op_from_u64(x: u64) -> Self;
  fn op_from_i64(x: i64) -> Self;
  fn op_to_u64(self) -> u64;
  fn op_to_i64(self) -> i64;

  fn op_not(self) -> Self;
  fn op_neg(self) -> Self;

  fn op_add(self, rhs: Self) -> (Self, bool);
  fn op_sub(self, rhs: Self) -> (Self, bool);
  fn op_mul(self, rhs: Self) -> (Self, bool);
  fn op_div(self, rhs: Self) -> Self;
  fn op_rem(self, rhs: Self) -> Self;

  fn op_bit_and(self, rhs: Self) -> Self;
  fn op_bit_or (self, rhs: Self) -> Self;
  fn op_bit_xor(self, rhs: Self) -> Self;

  fn op_shl(self, amt: u32) -> Self;
  fn op_shr(self, amt: u32) -> Self;
}  


/// Representation for bit-vectors of width 0.
#[derive(Clone,Copy,PartialEq,Eq,PartialOrd,Ord)]
pub struct U0 ();

impl Ops for U0 {
  const WIDTH: u32 = 0;

  fn op_from_u64(_: u64) -> Self { U0() }
  fn op_from_i64(_: i64) -> Self { U0() }
  fn op_to_u64(self) -> u64 { 0 }
  fn op_to_i64(self) -> i64 { 0 }

  fn op_not(self) -> Self { U0() }
  fn op_neg(self) -> Self { U0() }

  fn op_add(self, _rhs: Self) -> (Self, bool) { (U0(), false) }
  fn op_sub(self, _rhs: Self) -> (Self, bool) { (U0(), false) }
  fn op_mul(self, _rhs: Self) -> (Self, bool) { (U0(), false) }
  fn op_div(self, _rhs: Self) -> Self { U0() }
  fn op_rem(self, _rhs: Self) -> Self { U0() }

  fn op_bit_and(self, _rhs: Self) -> Self { U0() }
  fn op_bit_or (self, _rhs: Self) -> Self { U0() }
  fn op_bit_xor(self, _rhs: Self) -> Self { U0() }

  fn op_shl(self, _amt: u32)  -> Self { U0() }
  fn op_shr(self, _amt: u32)  -> Self { U0() }

}

impl fmt::Display for U0 {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "0")
  }
}

impl fmt::Debug for U0 {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "0")
  }
}

impl serde::Serialize for U0 {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_u64(0)
  }
}

macro_rules! MakeOps {
  ($ty: ty) => {
    impl Ops for $ty {
      const WIDTH: u32 = Self::BITS;

      fn op_from_u64(x: u64)  -> Self { x as Self }
      fn op_from_i64(x: i64)  -> Self { x as Self }
      fn op_to_u64(self)      -> u64 { self as u64 }
      fn op_to_i64(self)      -> i64 { self as i64 }

      fn op_not(self) -> Self { !self }
      fn op_neg(self) -> Self { self.wrapping_neg() }

      fn op_add(self, rhs: Self) -> (Self, bool) { self.overflowing_add(rhs) }
      fn op_sub(self, rhs: Self) -> (Self, bool) { self.overflowing_sub(rhs) }
      fn op_mul(self, rhs: Self) -> (Self, bool) { self.overflowing_mul(rhs) }
      fn op_div(self, rhs: Self) -> Self { self.wrapping_div(rhs) }
      fn op_rem(self, rhs: Self) -> Self { self.wrapping_rem(rhs) }

      fn op_bit_and(self, rhs: Self) -> Self { self & rhs }
      fn op_bit_or (self, rhs: Self) -> Self { self | rhs }
      fn op_bit_xor(self, rhs: Self) -> Self { self ^ rhs }

      fn op_shl(self, amt: u32) -> Self { self << amt }
      fn op_shr(self, amt: u32) -> Self { self >> amt }
    }  
  };
}
// Operators on basic types
MakeOps!(u8);
MakeOps!(u16);
MakeOps!(u32);
MakeOps!(u64);
MakeOps!(i8);
MakeOps!(i16);
MakeOps!(i32);
MakeOps!(i64);


/// A tag type to help compute [Word] representation.
pub struct Size<const S: bool, const N: u32> ();

/// A trait associating [Size] tags with concrete representations for [Word].
pub trait WordRep {
  type Rep: Ops;
  const PADDING: u32;
}

macro_rules! DefineRep {
  ($n: literal, $uty: ty, $ity: ty) => {
    impl WordRep for Size<false, $n> {
      type Rep = $uty;
      const PADDING: u32 = Self::Rep::WIDTH - $n;
    }
    impl WordRep for Size<true, $n> {
      type Rep = $ity;
      const PADDING: u32 = Self::Rep::WIDTH - $n;
    }
  }
}

DefineRep!(0, U0, U0);
DefineRep!(1, u8, i8);
DefineRep!(2, u8, i8);
DefineRep!(3, u8, i8);
DefineRep!(4, u8, i8);
DefineRep!(5, u8, i8);
DefineRep!(6, u8, i8);
DefineRep!(7, u8, i8);
DefineRep!(8, u8, i8);

DefineRep!( 9, u16, i16);
DefineRep!(10, u16, i16);
DefineRep!(11, u16, i16);
DefineRep!(12, u16, i16);
DefineRep!(13, u16, i16);
DefineRep!(14, u16, i16);
DefineRep!(15, u16, i16);
DefineRep!(16, u16, i16);

DefineRep!(17, u32, i32);
DefineRep!(18, u32, i32);
DefineRep!(19, u32, i32);
DefineRep!(20, u32, i32);
DefineRep!(21, u32, i32);
DefineRep!(22, u32, i32);
DefineRep!(23, u32, i32);
DefineRep!(24, u32, i32);
DefineRep!(25, u32, i32);
DefineRep!(26, u32, i32);
DefineRep!(27, u32, i32);
DefineRep!(28, u32, i32);
DefineRep!(29, u32, i32);
DefineRep!(30, u32, i32);
DefineRep!(31, u32, i32);
DefineRep!(32, u32, i32);

DefineRep!(33, u64, i64);
DefineRep!(34, u64, i64);
DefineRep!(35, u64, i64);
DefineRep!(36, u64, i64);
DefineRep!(37, u64, i64);
DefineRep!(38, u64, i64);
DefineRep!(39, u64, i64);
DefineRep!(40, u64, i64);
DefineRep!(41, u64, i64);
DefineRep!(42, u64, i64);
DefineRep!(43, u64, i64);
DefineRep!(44, u64, i64);
DefineRep!(45, u64, i64);
DefineRep!(46, u64, i64);
DefineRep!(47, u64, i64);
DefineRep!(48, u64, i64);
DefineRep!(49, u64, i64);
DefineRep!(50, u64, i64);
DefineRep!(51, u64, i64);
DefineRep!(52, u64, i64);
DefineRep!(53, u64, i64);
DefineRep!(54, u64, i64);
DefineRep!(55, u64, i64);
DefineRep!(56, u64, i64);
DefineRep!(57, u64, i64);
DefineRep!(58, u64, i64);
DefineRep!(59, u64, i64);
DefineRep!(60, u64, i64);
DefineRep!(61, u64, i64);
DefineRep!(62, u64, i64);
DefineRep!(63, u64, i64);
DefineRep!(64, u64, i64);


#[derive(Copy,Clone,PartialEq,Eq,PartialOrd,Ord)]
/// Generic representation for signed/unsigned bit-vectors of a given width.

#[repr(transparent)]
pub struct Word<const S: bool, const N: u32> where Size<S, N> : WordRep {
  rep: <Size<S,N> as WordRep>::Rep
}

impl<const S: bool, const N: u32> ddl::Type for Word<S,N> where Size<S,N>: WordRep {
  type B<'a> = Word<S,N>;
  fn bor(&self) -> Word<S,N> { *self }
}

impl<const S: bool, const N: u32> ddl::Clo for Word<S,N> where Size<S,N>: WordRep {
  type O = Word<S,N>;
  fn clo(self) -> Word<S,N> { self }
}


/// An unsigned [Word] of the given size.
pub type U<const N: u32> = Word<false, N>;

/// A signed [Word] of the given size.
pub type I<const N: u32> = Word<true, N>;


impl <const N: u32> U<N> where Size<false,N>: WordRep {
  pub const WIDTH: u32 = N;
  pub fn to_bits(self) -> U<N> { self }
  pub fn from_bits_unchecked(x: U<N>) -> Self { x }
}

impl <const N: u32> I<N> where Size<false, N>: WordRep, Size<true,N>: WordRep {
  pub const WIDTH: u32 = N;
  pub fn to_bits(self) -> U<N> { self.cast_to() }
  pub fn from_bits_unchecked(x: U<N>) -> Self { x.cast_to() }
}


// -----------------------------------------------------------------------------
// Standard Operations
//
// Wrapping arithmetic: these discard the overflow flag.
// Generated code does NOT use these for bounded integer types; it calls
// op_add/op_sub/op_mul directly to obtain the overflow flag and throw on
// overflow.  These trait impls exist only for convenience (e.g., literals,
// tests) and should not be relied upon for DDL checked-arithmetic semantics.
// -----------------------------------------------------------------------------

impl<const S: bool, const N: u32> ops::Add for Word<S,N> where Size<S, N>: WordRep {
  type Output = Self;
  fn add(self, rhs: Self) -> Self {
    Word { rep: self.rep.op_add(rhs.rep).0 }
  }
}

// Wrapping negation: generated code guards against negating non-zero unsigned
// values and negating minBound for signed types.
impl<const S: bool, const N: u32> ops::Neg for Word<S,N> where Size<S, N>: WordRep {
  type Output = Self;
  fn neg(self) -> Self {
    Word { rep: self.rep.op_neg() }
  }
}

impl<const S: bool, const N: u32> ops::Sub for Word<S,N> where Size<S,N>: WordRep {
  type Output = Self;
  fn sub(self, rhs: Self) -> Self {
    Word { rep: self.rep.op_sub(rhs.rep).0 }
  }
}

impl<const S: bool, const N: u32> ops::Mul for Word<S,N> where Size<S,N>: WordRep {
  type Output = Self;
  fn mul(self, rhs: Self) -> Self {
    Word { rep: self.rep.op_mul(rhs.rep.op_shr(Size::<S,N>::PADDING)).0 }
  }
}

// Truncating division and remainder: rounds toward zero.
// Generated code inserts a division-by-zero guard before calling these.
impl<const S: bool, const N: u32> ops::Div for Word<S,N> where Size<S, N>: WordRep {
  type Output = Self;
  fn div(self, rhs: Self) -> Self {
    let p = Size::<S,N>::PADDING;
    Word { rep: self.rep.op_div(rhs.rep.op_shr(p)).op_shr(p).op_shl(p) }
  }
}

impl<const S: bool, const N: u32> ops::Rem for Word<S,N> where Size<S,N>: WordRep {
  type Output = Self;
  fn rem(self, rhs: Self) -> Self {
    let p = Size::<S,N>::PADDING;
    Word { rep: self.rep.op_shr(p).op_rem(rhs.rep.op_shr(p)).op_shl(p) }
  }
}

impl<const S: bool, const N: u32> ops::Shl<usize> for Word<S,N> where Size<S,N>: WordRep {
  type Output = Self;
  fn shl(self, rhs: usize) -> Self {
    if rhs >= N as usize {
      Word { rep: Ops::op_from_u64(0) } 
    } else {
      Word { rep: self.rep.op_shl(rhs as u32) }
    }
  }
}

impl<const S: bool, const N: u32> ops::Shr<usize> for Word<S,N> where Size<S,N>: WordRep {
  type Output = Self;
  fn shr(self, rhs: usize) -> Self {
    if rhs >= N as usize {
      let z = Ops::op_from_u64(0);
      let p = Size::<S,N>::PADDING;
      Word { rep: if S && (self.rep < z) { z.op_not().op_shl(p) } else { z } }
    } else {
      Word { rep: self.rep.op_shr(rhs as u32) }
    }
  }
}

impl<const S: bool, const N: u32> ops::BitAnd for Word<S,N> where Size<S,N>: WordRep {
  type Output = Self;
  fn bitand(self, rhs: Self) -> Self {
    Word { rep: self.rep.op_bit_and(rhs.rep) }
  }
}

impl<const S: bool, const N: u32> ops::BitOr for Word<S,N> where Size<S,N>: WordRep {
  type Output = Self;
  fn bitor(self, rhs: Self) -> Self {
    Word { rep: self.rep.op_bit_or(rhs.rep) }
  }
}

impl<const S: bool, const N: u32> ops::BitXor for Word<S,N> where Size<S,N>: WordRep {
  type Output = Self;
  fn bitxor(self, rhs: Self) -> Self {
    Word { rep: self.rep.op_bit_xor(rhs.rep) }
  }
}

impl<const S: bool, const N: u32> ops::Not for Word<S,N> where Size<S, N>: WordRep {
  type Output = Self;
  fn not(self) -> Self {
    let p = Size::<S,N>::PADDING;
    Word { rep: self.rep.op_not().op_shr(p).op_shl(p) }
  }
}




impl <const N: u32> fmt::Display for Word<false,N> where Size<false,N>: WordRep {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    u64::from(*self).fmt(f)
  }
}

impl <const N: u32> fmt::Display for Word<true,N> where Size<true,N>: WordRep {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    i64::from(*self).fmt(f)
  }
}

impl <const N: u32> fmt::Debug for Word<false,N> where Size<false,N>: WordRep {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "U<{}>({:?})", N, u64::from(*self))
  }
}

impl <const N: u32> fmt::Debug for Word<true,N> where Size<true,N>: WordRep {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "I<{}>({:?})", N, i64::from(*self))
  }
}

impl <const N: u32> Serialize for Word<false,N> where Size<false,N>: WordRep {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_u64(u64::from(*self))
  }
}

impl <const N: u32> Serialize for Word<true,N> where Size<true,N>: WordRep {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_i64(i64::from(*self))
  }
}


// Coercions to- and from- standard Rust types
macro_rules! Coercions {
  ($ty: ty, $sign: literal, $via: ty, $from: ident, $to: ident) => {
    impl <const N: u32> From<$ty> for Word<$sign,N> where Size<$sign,N>: WordRep {
      fn from(x: $ty) -> Word<$sign,N> {
        let mut r: <Size<$sign,N> as WordRep>::Rep = Ops::$from(x as $via);
        r = r.op_shl(Size::<$sign,N>::PADDING);
        Word { rep: r }
      }
    }
    impl <const N: u32> From<Word<$sign,N>> for $ty where Size<$sign,N>: WordRep {
      fn from(x: Word<$sign,N>) -> $ty {
        let r = x.rep;
        r.op_shr(Size::<$sign,N>::PADDING).$to() as $ty
      }
    }  
  };
}

// To/from standard Rust types
Coercions!(u8,    false, u64, op_from_u64, op_to_u64);
Coercions!(u16,   false, u64, op_from_u64, op_to_u64);
Coercions!(u32,   false, u64, op_from_u64, op_to_u64);
Coercions!(u64,   false, u64, op_from_u64, op_to_u64);
Coercions!(usize, false, u64, op_from_u64, op_to_u64);
Coercions!(i8,    true,  i64, op_from_i64, op_to_i64);
Coercions!(i16,   true,  i64, op_from_i64, op_to_i64);
Coercions!(i32,   true,  i64, op_from_i64, op_to_i64);
Coercions!(i64,   true,  i64, op_from_i64, op_to_i64);
Coercions!(isize, true,  i64, op_from_i64, op_to_i64);

impl<const S: bool, const N: u32> Word<S,N> where Size<S,N>: WordRep {

  /// Coerce between various words.
  pub fn cast_to<const S1: bool, const N1: u32>(self) -> Word<S1,N1> where Size<S1,N1>: WordRep {
    let v = self.rep.op_shr(Size::<S,N>::PADDING).op_to_u64();
    Word { rep: <<Size<S1,N1> as WordRep>::Rep>::op_from_u64(v).op_shl(Size::<S1,N1>::PADDING) }
  }

  // Checked arithmetic: used by generated code for bounded integer types.
  // Returns (wrapped_result, overflow_flag).
  pub fn op_add(self, rhs: Self) -> (Self, bool) {
    let (r, overflow) = self.rep.op_add(rhs.rep);
    (Word { rep: r }, overflow)
  }

  pub fn op_sub(self, rhs: Self) -> (Self, bool) {
    let (r, overflow) = self.rep.op_sub(rhs.rep);
    (Word { rep: r }, overflow)
  }

  pub fn op_mul(self, rhs: Self) -> (Self, bool) {
    let (r, overflow) = self.rep.op_mul(rhs.rep.op_shr(Size::<S,N>::PADDING));
    (Word { rep: r }, overflow)
  }

}

impl<const N: u32> U<N> where Size<false,N>: WordRep {

  pub fn to_f32(self) -> f32 { u64::from(self) as f32 }
  pub fn to_f64(self) -> f64 { u64::from(self) as f64 }

  pub fn from_f32(v: f32) -> Self {
    if v.is_nan() || v <= 0.0 { return U::<N>::from(0u64) }
    let hi: u64 = if N >= 64 { u64::MAX } else { (1u64 << N) - 1 };
    if v >= (hi as f64 + 1.0) as f32 { return U::<N>::from(hi) }
    let i = v as u64;
    U::<N>::from(if i > hi { hi } else { i })
  }

  pub fn from_f64(v: f64) -> Self {
    if v.is_nan() || v <= 0.0 { return U::<N>::from(0u64) }
    let hi: u64 = if N >= 64 { u64::MAX } else { (1u64 << N) - 1 };
    if v >= hi as f64 + 1.0 { return U::<N>::from(hi) }
    let i = v as u64;
    U::<N>::from(if i > hi { hi } else { i })
  }
}

impl<const N: u32> I<N> where Size<false,N>: WordRep, Size<true,N>: WordRep {

  pub fn to_f32(self) -> f32 { i64::from(self) as f32 }
  pub fn to_f64(self) -> f64 { i64::from(self) as f64 }

  pub fn from_f32(v: f32) -> Self {
    if v.is_nan() { return I::<N>::from(0i64) }
    let lo: i64 = if N == 0 { 0 } else { -(1i64 << (N - 1)) };
    let hi: i64 = if N == 0 { 0 } else { (1i64 << (N - 1)) - 1 };
    if v <= lo as f32 - 1.0 { return I::<N>::from(lo) }
    if v >= hi as f32 + 1.0 { return I::<N>::from(hi) }
    let i = v as i64;
    I::<N>::from(i.clamp(lo, hi))
  }

  pub fn from_f64(v: f64) -> Self {
    if v.is_nan() { return I::<N>::from(0i64) }
    let lo: i64 = if N == 0 { 0 } else { -(1i64 << (N - 1)) };
    let hi: i64 = if N == 0 { 0 } else { (1i64 << (N - 1)) - 1 };
    if v <= lo as f64 - 1.0 { return I::<N>::from(lo) }
    if v >= hi as f64 + 1.0 { return I::<N>::from(hi) }
    let i = v as i64;
    I::<N>::from(i.clamp(lo, hi))
  }
}

#[macro_export]
macro_rules! cat {
  ($M: literal, $N: literal, $x: expr, $y: expr) => {
    $crate::U::<{$M+$N}>::from( (u64::from($x) << ($N as usize)) | u64::from($y))
  };
}

pub fn lcat<const M: u32, const N: u32>(x: U<M>, y: U<N>) -> U<M>
  where
  Size<false, M>: WordRep, Size<false,N>: WordRep, U<M>: From<U<N>> {
  (x << (N as usize)) | U::<M>::from(y)
}