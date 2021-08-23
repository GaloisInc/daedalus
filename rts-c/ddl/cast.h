#ifndef DDL_CAST_H
#define DDL_CAST_H

#include <ddl/number.h>
#include <ddl/integer.h>
#include <ddl/maybe.h>

namespace DDL {

template <int in, int out>
inline
UInt<out> uint_to_uint(UInt<in> x) {
  using Res = UInt<out>;
  return Res(typename Res::Rep{x.data});
}

template <int in, int out>
inline
UInt<out> sint_to_uint(SInt<in> x) {
  using Res = UInt<out>;
  return Res(typename Res::Rep{x.data});
}


template <int in, int out>
inline
SInt<out> uint_to_sint(UInt<in> x) {
  using Res = SInt<out>;
  return Res(typename Res::Rep{x.data});
}

template <int in, int out>
inline
SInt<out> sint_to_sint(SInt<in> x) {
  using Res = SInt<out>;
  return Res(typename Res::Rep{x.data});
}


// -----------------------------------------------------------------------------
// Integers

template <int in>
inline
Integer uint_to_integer(UInt<in> x) { return Integer(x.rep()); }

template <int in>
inline
Integer sint_to_integer(SInt<in> x) { return Integer(x.rep()); }


// borrow
template <int out>
inline
UInt<out> integer_to_uint(Integer x) {
  typename UInt<out>::Rep r;
  x.exportI(r);
  return UInt<out>(r);
}

// borrow
template <int out>
inline
SInt<out> integer_to_sint(Integer x) {
  typename SInt<out>::Rep r;
  x.exportI(r);
  return SInt<out>(r);
}



template <typename T>
inline
T refl_cast(T x) {
  if constexpr (hasRefs<T>()) { x.copy(); }
  return x;
}




// -----------------------------------------------------------------------------




template <int in, int out>
inline
Maybe<UInt<out>> uint_to_uint_maybe(UInt<in> x) {
  using Res = UInt<out>;
  if constexpr (out >= in) {
    return Maybe<Res>(uint_to_uint<in,out>(x));
  }
  UInt<in> lim = UInt<in>(Res::maxValRep());
  return x <= lim ? Maybe<Res>(uint_to_uint<in,out>(x))
                  : Maybe<Res>();
}

template <int in, int out>
inline
Maybe<SInt<out>> sint_to_sint_maybe(SInt<in> x) {
  using Res = SInt<out>;
  if constexpr (out >= in) {
    return Maybe<Res>(sint_to_sint<in,out>(x));
  }
  SInt<in> lower  = SInt<in>(Res::minValRep());
  SInt<in> upper  = SInt<in>(Res::maxValRep());
  return (lower <= x && x <= upper) ? Maybe<Res>(sint_to_sint<in,out>(x))
                                    : Maybe<Res>();
}


template <int in, int out>
inline
Maybe<SInt<out>> uint_to_sint_maybe(UInt<in> x) {
  using Res = SInt<out>;
  if constexpr (out > in) {
    return Maybe<Res>(uint_to_sint<in,out>(x));
  }
  UInt<in> upper = UInt<in>(Res::maxValRep());
  return (x <= upper) ? Maybe<Res>(uint_to_sint<in,out>(x))
                      : Maybe<Res>();
}

template <int in, int out>
inline
Maybe<UInt<out>> sint_to_uint_maybe(SInt<in> x) {
  using Res = UInt<out>;
  if (x < 0) return Maybe<Res>();
  if constexpr (out >= in) {
    return Maybe<Res>(sint_to_uint<in,out>(x));
  }
  SInt<in> upper = SInt<in>(Res::maxValRep());
  return (x <= upper) ? Maybe<Res>(sint_to_uint<in,out>(x)) : Maybe<Res>();
}


template <int in>
inline
Maybe<Integer> uint_to_integer_maybe(UInt<in> x) {
  return Maybe<Integer>(uint_to_integer<in>(x));
}

template <int in>
inline
Maybe<Integer> sint_to_integer_maybe(SInt<in> x) {
  return Maybe<Integer>(sint_to_integer<in>(x));
}




// XXX: Update
template <int out>
inline
Maybe<UInt<out>> integer_to_uint_maybe(Integer x) {
  using Res = UInt<out>;
  unsigned long val;
  static_assert(sizeof(unsigned long) * 8 >= out, "Unsupported cast");
  if (!x.isNatural() || !x.fitsULong()) goto NOPE;
  val = x.asULong();
  if (val <= Res::maxValRep()) {
    return Maybe<Res>(Res(val));
  }

NOPE:
  return Maybe<Res>();
}

// XXX: Update
template <int out>
inline
Maybe<SInt<out>> integer_to_sint_maybe(Integer x) {
  using Res = SInt<out>;
  long val;
  static_assert(sizeof(long) * 8 >= out, "Unsupported cast");
  if (!x.fitsSLong()) goto NOPE;
  val = x.asSLong();
  if (Res::minValRep() <= val && val <= Res::maxValRep()) {
    return Maybe<Res>(Res(val));
  }

NOPE:
  return Maybe<Res>();
}


template <typename T>
inline
Maybe<T> refl_cast_maybe(T x) {
  if constexpr (hasRefs<T>()) { x.copy(); }
  return Maybe<T>(x);
}



}

#endif

