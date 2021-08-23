#ifndef DDL_INTEGER_H
#define DDL_INTEGER_H

// #define QUICK_INTEGER 1



#ifdef QUICK_INTEGER
#include <ddl/int.h>
#else

#include <gmpxx.h>
#include <ddl/debug.h>
#include <ddl/boxed.h>
#include <ddl/size.h>

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

  // Mutable shift in place.
  // To be only used when we are the unique owners of this
  void mutShiftL(size_t amt) {
    mpz_class &r = getValue();
    r <<= amt;
  }

  // Mutable shift in place.
  // To be only used when we are the unique owners of this
  void mutShiftR(size_t amt) {
    mpz_class &r = getValue();
    r >>= amt;
  }

  // this |= x
  // To be only used when we are the unique owners of this
  void mutOr(unsigned long val) {
    mpz_class &r = getValue();
    r |= val;
  }

};

static inline
int compare(Integer x, Integer y) { return cmp(x.getValue(),y.getValue()); }

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
bool operator >  (Integer x, Integer y) { return x.getValue() >  y.getValue(); }

// borrow
static inline
bool operator >= (Integer x, Integer y) { return x.getValue() >= y.getValue(); }



// borrow
static inline
std::ostream& operator<<(std::ostream& os, Integer x) {
  return os << x.getValue();
}

// borrow
static inline
std::ostream& toJS(std::ostream& os, Integer x) {
  return os << std::dec << x.getValue();
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



// owned, unmanaged
static inline
Integer operator << (Integer x, Size iamt) {
  size_t amt = iamt.rep();
  mpz_class &v = x.getValue();
  if (x.refCount() == 1) { x.mutShiftL(amt); return x; }
  Integer y(v << amt);
  x.free();
  return y;
}

// owned, unmanaged
static inline
Integer operator >> (Integer x, Size iamt) {
  size_t amt = iamt.rep();
  mpz_class &v = x.getValue();
  if (x.refCount() == 1) { x.mutShiftR(amt); return x; }
  Integer y(v >> amt);
  x.free();
  return y;
}

// NOTE: lcat is in `number.h` to avoid dependency conflicts
// Temprary shift with UInt<64> are also there for the same reason



}
#endif



#endif


