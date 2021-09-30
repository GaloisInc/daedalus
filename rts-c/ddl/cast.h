#ifndef DDL_CAST_H
#define DDL_CAST_H

#include <ddl/number.h>
#include <ddl/integer.h>
#include <ddl/maybe.h>

namespace DDL {

template <Width in, Width out>
inline
UInt<out> uint_to_uint(UInt<in> x) { return UInt<out>(x.rep()); }

template <Width in, Width out>
inline
UInt<out> sint_to_uint(SInt<in> x) { return UInt<out>(x.rep()); }


template <Width in, Width out>
inline
SInt<out> uint_to_sint(UInt<in> x) { return SInt<out>(x.rep()); }

template <Width in, Width out>
inline
SInt<out> sint_to_sint(SInt<in> x) { return SInt<out>(x.rep()); }


// -----------------------------------------------------------------------------
// Integers

template <Width in>
inline
Integer uint_to_integer(UInt<in> x) { return Integer(x.rep()); }

template <Width in>
inline
Integer sint_to_integer(SInt<in> x) { return Integer(x.rep()); }


// borrow
template <Width out>
inline
UInt<out> integer_to_uint(Integer x) {
  typename UInt<out>::Rep r;
  x.exportI(r);
  return UInt<out>(r);
}

// borrow
template <Width out>
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




template <Width in, Width out>
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

template <Width in, Width out>
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


template <Width in, Width out>
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

template <Width in, Width out>
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


template <Width in>
inline
Maybe<Integer> uint_to_integer_maybe(UInt<in> x) {
  return Maybe<Integer>(uint_to_integer<in>(x));
}

template <Width in>
inline
Maybe<Integer> sint_to_integer_maybe(SInt<in> x) {
  return Maybe<Integer>(sint_to_integer<in>(x));
}




template <Width out>
inline
Maybe<UInt<out>> integer_to_uint_maybe(Integer x) {
  using Res = UInt<out>;
  if (x.isNatural() && mpz_sizeinbase(x.getValue().get_mpz_t(), 2) <= out) {
    typename Res::Rep v;
    x.exportI(v);
    return Maybe<Res>(Res{v});
  }
  return Maybe<Res>();
}


template <Width out>
inline
Maybe<SInt<out>> integer_to_sint_maybe(Integer x) {
  using Res = SInt<out>;
  mpz_class& c = x.getValue();

  if (mpz_fits_slong_p(c.get_mpz_t())) {
    long int v = mpz_get_si(c.get_mpz_t());
    return (Res::minValRep() <= v && v <= Res::maxValRep())
           ? Maybe<Res>(Res{static_cast<typename Res::Rep>(v)})
           : Maybe<Res>();
  }

  if constexpr (out <= 8 * sizeof(long)) return Maybe<Res>();
  else {
    // doesn't fit in a long, and out is a big type
    typename Res::Rep r;
    x.exportI(r);
    Integer check{r};
    bool ok = check == x;
    check.free();
    return ok ? Maybe<Res>(Res{r}) : Maybe<Res>();
  }
}



template <typename T>
inline
Maybe<T> refl_cast_maybe(T x) {
  if constexpr (hasRefs<T>()) { x.copy(); }
  return Maybe<T>(x);
}



}

#endif

