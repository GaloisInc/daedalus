#ifndef DDL_INTEGER_H
#define DDL_INTEGER_H

// #define QUICK_INTEGER



#ifdef QUICK_INTEGER
#include <ddl/int.h>
#else

#include <gmpxx.h>
#include <ddl/debug.h>
#include <ddl/boxed.h>
#include <ddl/size.h>
#include <ddl/number.h>

namespace DDL {

class Integer;

static bool operator <= (Integer x, Integer y);
static bool operator >= (Integer x, Integer y);

class Integer : public Boxed<mpz_class> {

  template <typename rep>
  rep toUnsigned() {
    mpz_t r;
    mpz_init(r);
    mpz_fdiv_r_2exp(r, getValue().get_mpz_t(),8*sizeof(rep));

    rep result = 0;
    mpz_export(&result, NULL, 1, sizeof(rep), 0, 0, r);
    mpz_clear(r);
    return result;
  }

  template <typename sign, typename usign>
  sign toSigned() {
    static_assert(sizeof(sign) == sizeof(usign));

    usign u = 0;
    mpz_t r;
    mpz_init(r);
    mpz_fdiv_r_2exp(r,getValue().get_mpz_t(),8*sizeof(sign));
    mpz_export(&u, NULL, 1, sizeof(sign), 0, 0, r);
    mpz_clear(r);

    sign result = static_cast<usign>(u);
    return result;
  }



public:
  Integer()                 : Boxed<mpz_class>()               {}
  Integer(const char* str)  : Boxed<mpz_class>(mpz_class(str)) {}

  // unsigned constructors
  Integer(uint8_t x)  : Boxed<mpz_class>(static_cast<unsigned long>(x)) {}
  Integer(uint16_t x) : Boxed<mpz_class>(static_cast<unsigned long>(x)) {}
  Integer(uint32_t x) : Boxed<mpz_class>(static_cast<unsigned long>(x)) {}
  Integer(uint64_t x) : Boxed<mpz_class>(static_cast<unsigned long>(x)) {
    if constexpr (sizeof(uint64_t) > sizeof(unsigned long))
      if (x > std::numeric_limits<unsigned long>::max()) {
        mpz_import(getValue().get_mpz_t(), 1, 1, 8, 0, 0, &x);
      }
  }
  Integer(Size x) : Boxed<mpz_class>(static_cast<unsigned long>(x.rep())) {
    if constexpr (sizeof(size_t) > sizeof(unsigned long))
      if (x > std::numeric_limits<unsigned long>::max()) {
        size_t v = x.rep();
        mpz_import(getValue().get_mpz_t(), 1, 1, 8, 0, 0, &v);
      }
  }

  // signed constructors
  Integer(int8_t x)  : Boxed<mpz_class>(static_cast<long>(x)) {}
  Integer(int16_t x) : Boxed<mpz_class>(static_cast<long>(x)) {}
  Integer(int32_t x) : Boxed<mpz_class>(static_cast<long>(x)) {}
  Integer(int64_t x) : Boxed<mpz_class>(static_cast<long>(x)) {
    if constexpr (sizeof(int64_t) > sizeof(long)) {
      if (x > std::numeric_limits<long>::max()) {
        mpz_import(getValue().get_mpz_t(), 1, 1, 8, 0, 0, &x);
      } else
      if (x < std::numeric_limits<long>::min()) {
        mpz_class& c = getValue();
        uint64_t v = std::numeric_limits<uint64_t>::max() -
                     static_cast<uint64_t>(x);
        mpz_import(c.get_mpz_t(), 1, 1, 8, 0, 0, &v);
        c = -c - 1;
      }
    }
  }


  Integer(Boxed<mpz_class> x) : Boxed<mpz_class>(x)              {}
  Integer(mpz_class &&x)      : Boxed<mpz_class>(std::move(x))   {}

  Integer(double x) : Boxed<mpz_class>(x) {}
  Integer(float  x) : Boxed<mpz_class>(static_cast<double>(x)) {}

  bool isNatural() { return sgn(getValue()) >= 0; }

  void exportI(uint8_t &x)  { x = toUnsigned<uint8_t>(); }
  void exportI(uint16_t &x) { x = toUnsigned<uint16_t>(); }
  void exportI(uint32_t &x) { x = toUnsigned<uint32_t>(); }
  void exportI(uint64_t &x) { x = toUnsigned<uint64_t>(); }

  void exportI(int8_t &x)  { x = toSigned<int8_t,uint8_t>(); }
  void exportI(int16_t &x) { x = toSigned<int16_t,uint16_t>(); }
  void exportI(int32_t &x) { x = toSigned<int32_t,uint32_t>(); }
  void exportI(int64_t &x) { x = toSigned<int64_t,uint64_t>(); }

  // Used for casting
  double asDouble() { return getValue().get_d(); }

  // Used in array
  Size asSize() { return Size{ toUnsigned<size_t>() }; }


  // Mutable shift in place.
  // To be only used when we are the unique owners of this
  void mutShiftL(Size amt) {
    mpz_class &r = getValue();
    r <<= amt.rep();
  }

  // Mutable shift in place.
  // To be only used when we are the unique owners of this
  void mutShiftR(Size amt) {
    mpz_class &r = getValue();
    r >>= amt.rep();
  }

};


static inline
int compare(Integer x, Integer y) { return cmp(x.getValue(),y.getValue()); }

static inline
int compare(Integer x, uint32_t y) {
  static_assert(sizeof(uint32_t) <= sizeof(unsigned long));
  return mpz_cmp_ui(x.getValue().get_mpz_t(), y);
}

static inline
int compare(Integer x, int32_t y) {
  static_assert(sizeof(int32_t) <= sizeof(long));
  return mpz_cmp_si(x.getValue().get_mpz_t(), y);
}

static inline
int compare(Integer x, uint64_t y) {
  if constexpr (sizeof(uint64_t) <= sizeof(unsigned long)) {
    return mpz_cmp_ui(x.getValue().get_mpz_t(), y);
  } else {
    if (y <= std::numeric_limits<unsigned long>::max()) {
      return mpz_cmp_ui(x.getValue().get_mpz_t(),
                        static_cast<unsigned long>(y));
    } else {
      Integer i{y};
      int res = compare(x,i);
      i.free();
      return res;
    }
  }
}

static inline
int compare(Integer x, int64_t y) {
  if constexpr (sizeof(int64_t) <= sizeof(long)) {
    return mpz_cmp_si(x.getValue().get_mpz_t(), y);
  } else {
    if (y <= std::numeric_limits<long>::max()) {
      return mpz_cmp_si(x.getValue().get_mpz_t(), static_cast<long>(y));
    } else {
      Integer i{y};
      int res = compare(x,i);
      i.free();
      return res;
    }
  }
}





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
Integer operator << (Integer x, Size amt) {
  mpz_class &v = x.getValue();
  if (x.refCount() == 1) { x.mutShiftL(amt); return x; }
  Integer y(v << amt.rep());
  x.free();
  return y;
}

// owned, unmanaged
static inline
Integer operator >> (Integer x, Size amt) {
  mpz_class &v = x.getValue();
  if (x.refCount() == 1) { x.mutShiftR(amt); return x; }
  Integer y(v >> amt.rep());
  x.free();
  return y;
}


// owned
static inline
Integer operator | (Integer x, Integer y) {
  mpz_class &xv = x.getValue();
  mpz_class &yv = y.getValue();
  if (x.refCount() == 1) { xv |= yv;     y.free(); return x; }
  if (y.refCount() == 1) { yv = xv | yv; x.free(); return y; }
  Integer z(xv | yv);
  x.free(); y.free();
  return z;
}

// owned
static inline
Integer operator & (Integer x, Integer y) {
  mpz_class &xv = x.getValue();
  mpz_class &yv = y.getValue();
  if (x.refCount() == 1) { xv &= yv;     y.free(); return x; }
  if (y.refCount() == 1) { yv = xv & yv; x.free(); return y; }
  Integer z(xv & yv);
  x.free(); y.free();
  return z;
}


// owned
static inline
Integer operator ^ (Integer x, Integer y) {
  mpz_class &xv = x.getValue();
  mpz_class &yv = y.getValue();
  if (x.refCount() == 1) { xv ^= yv;     y.free(); return x; }
  if (y.refCount() == 1) { yv = xv ^ yv; x.free(); return y; }
  Integer z(xv ^ yv);
  x.free(); y.free();
  return z;
}


// owned, unmanaged
// XXX: remove in favor of size
static inline
Integer operator << (Integer x, UInt<64> iamt) {
  return x << Size::from(iamt.rep());
}

// owned, unmanaged
// XXX: remove in favor of size
static inline
Integer operator >> (Integer x, UInt<64> iamt) {
  return x >> Size::from(iamt.rep());
}

template <Width b>
Integer lcat(Integer x, UInt<b> y) { return (x << Size{b}) | Integer(y.rep()); }








// NOTE: lcat is in `number.h` to avoid dependency conflicts
// Temprary shift with UInt<64> are also there for the same reason



}
#endif



#endif


