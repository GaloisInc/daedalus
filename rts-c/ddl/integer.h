#ifndef DDL_INTEGER_H
#define DDL_INTEGER_H

#include <gmpxx.h>
#include <ddl/boxed.h>

namespace DDL {

class Integer : public Boxed<mpz_class> {

public:
  Integer()                   : Boxed<mpz_class>()               {}
  Integer(const char* str)    : Boxed<mpz_class>(mpz_class(str)) {}
  Integer(unsigned long x)    : Boxed<mpz_class>(x)              {}
  Integer(long x)             : Boxed<mpz_class>(x)              {}
  Integer(Boxed<mpz_class> x) : Boxed<mpz_class>(x)              {}
  Integer(mpz_class &&x)      : Boxed<mpz_class>(std::move(x))   {}

  bool isNatural() { return sgn(getValue()) >= 0; }

  // assumes we know things will fit
  unsigned long asULong()    { return getValue().get_ui(); }
  long          asSLong()    { return getValue().get_si(); }

  bool          fitsULong()  { return getValue().fits_ulong_p(); }
  bool          fitsSLong()  { return getValue().fits_slong_p(); }
};

// borrow
static inline
bool operator == (Integer x, Integer y) { return x.getValue() == y.getValue(); }

// borrow
static inline
bool operator != (Integer x, Integer y) { return x.getValue() != y.getValue(); }

// borrow
static inline
bool operator <  (Integer x, Integer y) { return x.getValue() <  y.getValue(); }

// borrow
static inline
bool operator <= (Integer x, Integer y) { return x.getValue() <= y.getValue(); }

// borrow
static inline
std::ostream& operator<<(std::ostream& os, Integer x) {
  os << x.getValue();
  return os;
}


template <int owned>  // bitmask for ownership of argument
static inline
Integer add(Integer x, Integer y) {
  mpz_class &xv = x.getValue();
  mpz_class &yv = y.getValue();
  if constexpr (owned & 1) {
    std::cout << "testing left" << std::endl;
    if (x.refCount() == 1) {
      xv += yv;
      if constexpr (owned & 2) y.free();
      return x;
    }
  }

  if constexpr (owned & 2) {
    std::cout << "testing right" << std::endl;
    if (y.refCount() == 1) {
      yv += xv;
      if constexpr (owned & 1) x.free();
      return y;
    }
  }

  Integer z(xv + yv);
  if constexpr (owned & 1) x.free();
  if constexpr (owned & 2) y.free();
  return z;
}


// owned
static inline
Integer operator + (Integer x, Integer y) {
  mpz_class &xv = x.getValue();
  mpz_class &yv = y.getValue();
  if (x.refCount() == 1) { xv += yv; y.free(); return x; }
  if (y.refCount() == 1) { yv += xv; x.free(); return y; }
  Integer z(xv + yv);
  x.free(); y.free();
  return z;
}

// owned
static inline
Integer operator - (Integer x, Integer y) {
  mpz_class &xv = x.getValue();
  mpz_class &yv = y.getValue();
  if (x.refCount() == 1) { xv -= yv; y.free(); return x; }
  if (y.refCount() == 1) { yv = xv - yv; x.free(); return y; }
  Integer z(xv - yv);
  x.free(); y.free();
  return z;
}

// owned
static inline
Integer operator * (Integer x, Integer y) {
  mpz_class &xv = x.getValue();
  mpz_class &yv = y.getValue();
  if (x.refCount() == 1) { xv *= yv; y.free(); return x; }
  if (y.refCount() == 1) { yv *= xv; x.free(); return y; }
  Integer z(xv * yv);
  x.free(); y.free();
  return z;
}

// XXX: Check for division by 0?
// owned
static inline
Integer operator / (Integer x, Integer y) {
  mpz_class &xv = x.getValue();
  mpz_class &yv = y.getValue();
  if (x.refCount() == 1) { xv /= yv;     y.free(); return x; }
  if (y.refCount() == 1) { yv = xv / yv; x.free(); return y; }
  Integer z(xv / yv);
  x.free(); y.free();
  return z;
}


// XXX: Check for division by 0?
// owned
static inline
Integer operator % (Integer x, Integer y) {
  mpz_class &xv = x.getValue();
  mpz_class &yv = y.getValue();
  if (x.refCount() == 1) { xv %= yv;     y.free(); return x; }
  if (y.refCount() == 1) { yv = xv % yv; x.free(); return y; }
  Integer z(xv % yv);
  x.free(); y.free();
  return z;
}

static inline
// owned
Integer operator - (Integer x) {
  mpz_class &v = x.getValue();
  if (x.refCount() == 1) { v = -v; return x; }
  Integer y(-v);
  x.free();
  return y;
}


}



#endif


