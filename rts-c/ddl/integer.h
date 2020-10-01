#ifndef DDL_INTEGER
#define DDL_INTEGER

#include <gmpxx.h>
#include <ddl/boxed.h>

namespace DDL {

class Integer : public Boxed<mpz_class> {

public:
  Integer(const char* str)    : Boxed<mpz_class>(mpz_class(str)) {}
  Integer()                   : Boxed<mpz_class>()               {}
  Integer(Boxed<mpz_class> x) : Boxed<mpz_class>(x)              {}
  Integer(mpz_class &&x)      : Boxed<mpz_class>(std::move(x))   {}
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


