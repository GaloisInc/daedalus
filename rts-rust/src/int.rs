use crate as ddl;
use crate::Type;
use num_bigint::BigInt;
use num_traits::{FromPrimitive, ToPrimitive};
use serde::{Serialize, Serializer};
use std::fmt;
use std::ops;

/// A wrapper type for arbitrary-precision integers
#[repr(transparent)]
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Int(BigInt);

impl Int {
    pub const ZERO: Int = Int(BigInt::ZERO);

    /// Construct an Int from big-endian signed bytes.
    pub fn from_signed_bytes_be(bytes: &[u8]) -> Int {
        Int(BigInt::from_signed_bytes_be(bytes))
    }

    pub fn lcat<const N: u32>(self, x: ddl::U<N>) -> Self
    where ddl::Size<false,N>: ddl::WordRep
    {
      (self << (N as usize)) | Self::from(x)
    }

    /// Try to convert to a u64.
    pub fn try_to_unsigned(&self) -> Option<u64> { (&self.0).try_into().ok() }

    /// Try to convert to a i64.
    pub fn try_to_signed(&self) -> Option<i64> { (&self.0).try_into().ok() }

    pub fn from_f32(v: f32) -> Self {
      if v.is_nan() || v.is_infinite() { return Int(BigInt::ZERO) }
      match BigInt::from_f32(v.trunc()) {
        Some(n) => Int(n),
        None    => Int(BigInt::ZERO),
      }
    }

    pub fn from_f64(v: f64) -> Self {
      if v.is_nan() || v.is_infinite() { return Int(BigInt::ZERO) }
      match BigInt::from_f64(v.trunc()) {
        Some(n) => Int(n),
        None    => Int(BigInt::ZERO),
      }
    }

    pub fn to_f32(&self) -> f32 {
      self.0.to_f32().unwrap_or(if self.0 < BigInt::ZERO { f32::NEG_INFINITY } else { f32::INFINITY })
    }

    pub fn to_f64(&self) -> f64 {
      self.0.to_f64().unwrap_or(if self.0 < BigInt::ZERO { f64::NEG_INFINITY } else { f64::INFINITY })
    }
}



// Display and Debug traits - delegate to BigInt
impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

// Serialize as a decimal string representation
impl Serialize for Int {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.0.to_string())
    }
}

// Arithmetic operations
impl ops::Add for Int {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Int(self.0 + rhs.0)
    }
}

impl ops::Sub for Int {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Int(self.0 - rhs.0)
    }
}

impl ops::Mul for Int {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        Int(self.0 * rhs.0)
    }
}

impl ops::Div for Int {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        Int(self.0 / rhs.0)
    }
}

impl ops::Rem for Int {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        Int(self.0 % rhs.0)
    }
}

impl ops::Neg for Int {
    type Output = Self;
    fn neg(self) -> Self {
        Int(-self.0)
    }
}

// Bitwise operations
impl ops::BitAnd for Int {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Int(self.0 & rhs.0)
    }
}

impl ops::BitOr for Int {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Int(self.0 | rhs.0)
    }
}

impl ops::BitXor for Int {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self {
        Int(self.0 ^ rhs.0)
    }
}

impl ops::Not for Int {
    type Output = Self;
    fn not(self) -> Self {
        Int(!self.0)
    }
}

// Shift operations
impl ops::Shl<usize> for Int {
    type Output = Self;
    fn shl(self, rhs: usize) -> Self {
        Int(self.0 << rhs)
    }
}

impl ops::Shr<usize> for Int {
    type Output = Self;
    fn shr(self, rhs: usize) -> Self {
        Int(self.0 >> rhs)
    }
}

// Conversions from Word types to Int
// Unsigned words
impl<const N: u32> From<ddl::U<N>> for Int
where
    ddl::number::Size<false, N>: ddl::number::WordRep,
{
    fn from(w: ddl::U<N>) -> Self {
        Int(BigInt::from(u64::from(w)))
    }
}

// Signed words
impl<const N: u32> From<ddl::I<N>> for Int
where
    ddl::number::Size<true, N>: ddl::number::WordRep,
{
    fn from(w: ddl::I<N>) -> Self {
        Int(BigInt::from(i64::from(w)))
    }
}

// Helper function to extract low 64 bits from Int with wrap-around semantics
fn int_to_u64_wrapped(i: &BigInt) -> u64 {
    use num_bigint::Sign;
    let sign = i.sign();
    let low_bits_magnitude = i.iter_u64_digits().next().unwrap_or(0);
    if sign == Sign::Minus && low_bits_magnitude != 0 {
        // For negative: compute -x mod 2^64 = 2^64 - x
        0u64.wrapping_sub(low_bits_magnitude)
    } else {
        low_bits_magnitude
    }
}

// Conversions from Int to Word types (with wrap-around/modulo semantics)
// Unsigned words
impl<const N: u32> From<Int> for ddl::U<N>
where
    ddl::number::Size<false, N>: ddl::number::WordRep,
{
    fn from(i: Int) -> Self {
        ddl::U::from(int_to_u64_wrapped(&i.0))
    }
}

// Conversions from Int to Word types (with wrap-around/modulo semantics)
// Unsigned words
impl<const N: u32> From<&Int> for ddl::U<N>
where
    ddl::number::Size<false, N>: ddl::number::WordRep,
{
    fn from(i: &Int) -> Self {
        ddl::U::from(int_to_u64_wrapped(&i.0))
    }
}



// Signed words
impl<const N: u32> From<Int> for ddl::I<N>
where
    ddl::number::Size<true, N>: ddl::number::WordRep,
{
    fn from(i: Int) -> Self {
        ddl::I::from(int_to_u64_wrapped(&i.0) as i64)
    }
}

// Signed words
impl<const N: u32> From<&Int> for ddl::I<N>
where
    ddl::number::Size<true, N>: ddl::number::WordRep,
{
    fn from(i: &Int) -> Self {
        ddl::I::from(int_to_u64_wrapped(&i.0) as i64)
    }
}


// Conversions from standard integer types to Int
impl From<u8> for Int {
    fn from(x: u8) -> Self {
        Int(BigInt::from(x))
    }
}

impl From<u16> for Int {
    fn from(x: u16) -> Self {
        Int(BigInt::from(x))
    }
}

impl From<u32> for Int {
    fn from(x: u32) -> Self {
        Int(BigInt::from(x))
    }
}

impl From<u64> for Int {
    fn from(x: u64) -> Self {
        Int(BigInt::from(x))
    }
}

impl From<i8> for Int {
    fn from(x: i8) -> Self {
        Int(BigInt::from(x))
    }
}

impl From<i16> for Int {
    fn from(x: i16) -> Self {
        Int(BigInt::from(x))
    }
}

impl From<i32> for Int {
    fn from(x: i32) -> Self {
        Int(BigInt::from(x))
    }
}

impl From<i64> for Int {
    fn from(x: i64) -> Self {
        Int(BigInt::from(x))
    }
}

// Implement the DDL Type trait using by_ref pattern
ddl::by_ref!(Int);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::number::{Size, WordRep, U, I};

    fn cvt_u(x: Int, n: usize) -> Int {
      let m = Int::from(1) << n;
      let mut r = x % m.clone();
      if r < Int::ZERO {
        r = r + m;
      }
      r
    }

    fn cvt_s(x: Int, n: usize) -> Int {
      let half = Int::from(1) << (n - 1);
      cvt_u(x + half.clone(), n) - half
   }

    fn check_cvt<const N: u32>(i: Int)
    where
        Size<false, N>: WordRep,
        Size<true, N>: WordRep,
        Int: From<U<N>> + From<I<N>>,
    {
      let u = U::<N>::from(i.clone());
      assert_eq!(Int::from(u), cvt_u(i.clone(), N as usize), "Incorrect Int -> U");
      let s = I::<N>::from(i.clone());
      assert_eq!(Int::from(s), cvt_s(i, N as usize), "Incorrect Int -> I");
    }
    #[test]
    fn test_int_to_word_conversions() {
        // Small values for various widths
        check_cvt::<1>(Int::from(0u64));
        check_cvt::<1>(Int::from(1u64));
        check_cvt::<1>(Int::from(2u64));
        check_cvt::<1>(Int::from(-1i64));

        check_cvt::<3>(Int::from(0u64));
        check_cvt::<3>(Int::from(7i64));
        check_cvt::<3>(Int::from(-4i64));
        check_cvt::<3>(Int::from(100i64));

        check_cvt::<5>(Int::from(0u64));
        check_cvt::<5>(Int::from(31u64));
        check_cvt::<5>(Int::from(32u64));
        check_cvt::<5>(Int::from(1000u64));

        check_cvt::<7>(Int::from(0i64));
        check_cvt::<7>(Int::from(63i64));
        check_cvt::<7>(Int::from(-64i64));
        check_cvt::<7>(Int::from(-1i64));

        // Standard sizes
        check_cvt::<8>(Int::from(0u64));
        check_cvt::<8>(Int::from(255u64));
        check_cvt::<8>(Int::from(256u64));
        check_cvt::<8>(Int::from(-1i64));
        check_cvt::<8>(Int::from(127i64));
        check_cvt::<8>(Int::from(-128i64));
        check_cvt::<8>(Int::from(-1000i64));

        // Unusual widths
        check_cvt::<13>(Int::from(0u64));
        check_cvt::<13>(Int::from(8191u64));
        check_cvt::<13>(Int::from(8192u64));
        check_cvt::<13>(Int::from(100000u64));

        check_cvt::<17>(Int::from(0i64));
        check_cvt::<17>(Int::from(65535i64));
        check_cvt::<17>(Int::from(-65536i64));

        check_cvt::<24>(Int::from(0u64));
        check_cvt::<24>(Int::from(16777215u64));
        check_cvt::<24>(Int::from(16777216u64));
        check_cvt::<24>(Int::from(u64::MAX));

        check_cvt::<33>(Int::from(0i64));
        check_cvt::<33>(Int::from(i32::MAX));
        check_cvt::<33>(Int::from(i32::MIN));

        check_cvt::<47>(Int::from(0u64));
        check_cvt::<47>(Int::from(u32::MAX as u64));
        check_cvt::<47>(Int::from(u64::MAX));

        // 64-bit widths
        check_cvt::<64>(Int::from(0u64));
        check_cvt::<64>(Int::from(u64::MAX));
        check_cvt::<64>(Int::from(-1i64));
        check_cvt::<64>(Int::from(i64::MAX));
        check_cvt::<64>(Int::from(i64::MIN));

        // Large Int values (> 64 bits)
        let large_positive = Int::from(u64::MAX) + Int::from(1000u64);
        check_cvt::<8>(large_positive.clone());
        check_cvt::<16>(large_positive.clone());
        check_cvt::<32>(large_positive.clone());
        check_cvt::<64>(large_positive.clone());
        check_cvt::<13>(large_positive.clone());
        check_cvt::<47>(large_positive);

        let large_negative = -(Int::from(u64::MAX) + Int::from(5000u64));
        check_cvt::<8>(large_negative.clone());
        check_cvt::<24>(large_negative.clone());
        check_cvt::<33>(large_negative.clone());
        check_cvt::<64>(large_negative);

        // Very large Int (2^100)
        let huge = Int::from(1u64) << 100;
        check_cvt::<1>(huge.clone());
        check_cvt::<7>(huge.clone());
        check_cvt::<16>(huge.clone());
        check_cvt::<32>(huge.clone());
        check_cvt::<64>(huge);

        // Very large negative Int (-2^100)
        let huge_neg = -(Int::from(1u64) << 100);
        check_cvt::<3>(huge_neg.clone());
        check_cvt::<5>(huge_neg.clone());
        check_cvt::<13>(huge_neg.clone());
        check_cvt::<24>(huge_neg.clone());
        check_cvt::<64>(huge_neg);

        // Edge case: 2^64
        let pow64 = Int::from(1u64) << 64;
        check_cvt::<8>(pow64.clone());
        check_cvt::<16>(pow64.clone());
        check_cvt::<32>(pow64.clone());
        check_cvt::<64>(pow64);
    }

    #[test]
    fn test_try_to_unsigned() {
        // Valid conversions within u64 range
        assert_eq!(Int::from(0u64).try_to_unsigned(), Some(0u64));
        assert_eq!(Int::from(255u64).try_to_unsigned(), Some(255u64));
        assert_eq!(Int::from(256u64).try_to_unsigned(), Some(256u64));
        assert_eq!(Int::from(65535u64).try_to_unsigned(), Some(65535u64));
        assert_eq!(Int::from(65536u64).try_to_unsigned(), Some(65536u64));
        assert_eq!(Int::from(u64::MAX).try_to_unsigned(), Some(u64::MAX));

        // Negative values fail
        assert!(Int::from(-1i64).try_to_unsigned().is_none());
        assert!(Int::from(-128i64).try_to_unsigned().is_none());

        // Large Int values beyond u64 range
        let large = Int::from(u64::MAX) + Int::from(1000u64);
        assert!(large.try_to_unsigned().is_none());

        // Verify actual values
        assert_eq!(Int::from(42u64).try_to_unsigned().unwrap(), 42u64);
    }

    #[test]
    fn test_try_to_signed() {
        // Valid conversions within i64 range
        assert_eq!(Int::from(-128i64).try_to_signed(), Some(-128i64));
        assert_eq!(Int::from(127i64).try_to_signed(), Some(127i64));
        assert_eq!(Int::from(0i64).try_to_signed(), Some(0i64));
        assert_eq!(Int::from(-32768i64).try_to_signed(), Some(-32768i64));
        assert_eq!(Int::from(32767i64).try_to_signed(), Some(32767i64));
        assert_eq!(Int::from(i64::MIN).try_to_signed(), Some(i64::MIN));
        assert_eq!(Int::from(i64::MAX).try_to_signed(), Some(i64::MAX));

        // Large Int values beyond i64 range
        let large_pos = Int::from(i64::MAX) + Int::from(1000u64);
        let large_neg = Int::from(i64::MIN) - Int::from(1000u64);
        assert!(large_pos.try_to_signed().is_none());
        assert!(large_neg.try_to_signed().is_none());

        // Verify actual values
        assert_eq!(Int::from(42i64).try_to_signed().unwrap(), 42i64);
        assert_eq!(Int::from(-42i64).try_to_signed().unwrap(), -42i64);
    }
}
