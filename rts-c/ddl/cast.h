#ifndef DDL_CAST
#define DDL_CAST

#include <ddl/number.h>
#include <ddl/integer.h>
#include <ddl/maybe.h>

namespace DDL {

template <int in, int out>
inline
UInt<out> uint_to_uint(UInt<in> x) {
  return UInt<out>(typename UInt<out>::Rep(x.data));
}

template <int in, int out>
inline
Maybe<UInt<out>> uint_to_uint_maybe(UInt<in> x) {
  UInt<in> lim = uint_to_uint<out,in>(UInt<out>::maxVal());
  return x <= lim ? Maybe<UInt<out>>(uint_to_uint<in,out>(x))
                  : Maybe<UInt<out>>();
}


template <int in>
inline
Integer uint_to_integer(UInt<in> x) {
  static_assert(sizeof(unsigned long) * 8 >= in, "Unsupported cast");
  return Integer((unsigned long)(x.rep()));
}

template <int in>
inline
Integer sint_to_integer(SInt<in> x) {
  static_assert(sizeof(long) * 8 >= in, "Unsupported cast");
  return Integer((long)x.rep());
}


template <int out>
inline
Maybe<UInt<out>> integer_to_uint_maybe(Integer x) {
  unsigned long val;
  static_assert(sizeof(unsigned long) * 8 >= out, "Unsupported cast");
  if (!x.isNatural() || !x.fitsULong()) goto NOPE;
  val = x.asULong();
  if (val <= UInt<out>::maxVal().data)
    return Maybe<UInt<out>>(UInt<out>(val));

NOPE:
  return Maybe<UInt<out>>();
}

#if 0
template <int out>
inline
Maybe<UInt<out>> integer_to_sint_maybe(Integer x) {
  Integer ulim = uint_integer<out>(UInt<out>::maxVal());
  if (x.isNatural() && x <= lim) {
    return Maybe<UInt<out>>(UInt<out>(x.asULong()));
  } else {
    return Maybe<UInt<out>>();
  }
}
#endif


}

#endif

