#ifndef DDL_NUMBER_H
#define DDL_NUMBER_H

#include <cstdint>
#include <cassert>
#include <type_traits>
#include <iostream>
#include <ios>
#include <cmath>

#include <ddl/value.h>
#include <ddl/bool.h>
#include <ddl/integer.h>
#include <ddl/size.h>


namespace DDL {


// Unsigned --------------------------------------------------------------------

template <size_t w>
struct UInt : public Value {
  static_assert(w <= 64, "UInt larger than 64 not supported.");

  using Rep =
    typename std::conditional < (w <= 8),   uint8_t,
    typename std::conditional < (w <= 16),  uint16_t,
    typename std::conditional < (w <= 32),  uint32_t,
                                            uint64_t
    >::type>::type>::type;

  Rep data;

public:

  // For uninitialized values
  UInt()      : data(0) {}

  // For generating literals
  UInt(Rep d) : data(d) {}

  // This is for `a # b`
  template <size_t a, size_t b>
  UInt(UInt<a> x, UInt<b> y) : data((Rep(x.data) << b) | y.rep()) {
    static_assert(a + b == w);
  }

  Rep rep() {
    if constexpr (w == 8 || w == 16 || w == 32 || w == 64) return data;
    if constexpr (w < 8)   return data & (UINT8_MAX  >> ( 8-w));
    if constexpr (w < 16)  return data & (UINT16_MAX >> (16-w));
    if constexpr (w < 32)  return data & (UINT32_MAX >> (32-w));
    return                        data & (UINT64_MAX >> (64-w));
  }

  constexpr static Rep maxValRep() {
    if constexpr (w ==  8) return UINT8_MAX;  else
    if constexpr (w == 16) return UINT16_MAX; else
    if constexpr (w == 32) return UINT32_MAX; else
    if constexpr (w == 64) return UINT64_MAX; else
    return (1 << w) - 1;
  }


  UInt operator + (UInt x) { return UInt(data + x.data); }
  UInt operator - (UInt x) { return UInt(data - x.data); }
  UInt operator * (UInt x) { return UInt(data * x.data); }
  UInt operator % (UInt x) { Rep xv = x.rep();
                             assert(xv /= 0);
                             return UInt(rep() % xv); }
  UInt operator / (UInt x) { Rep xv = x.rep();
                             assert(xv /= 0);
                             return UInt(rep() / xv); }
  UInt operator - ()       { return UInt(-data); }
  UInt operator ~ ()       { return UInt(~data); }

  UInt operator | (UInt x) { return UInt(data | x.data); }
  UInt operator & (UInt x) { return UInt(data & x.data); }
  UInt operator ^ (UInt x) { return UInt(data ^ x.data); }

  // XXX: remove in favor of Size
  UInt operator << (UInt<64> x) {
    uint64_t n = x.rep();
    return n >= w? UInt(0) : UInt(data << n);
  }

  // XXX: remove in favor of Size
  UInt operator >> (UInt<64> x) {
    uint64_t n = x.rep();
    return n >= w? UInt(0) : UInt(data >> n);
  }

  UInt operator << (Size x) {
    size_t n = x.rep();
    return n >= w? UInt(0) : UInt(data << n);
  }

  UInt operator >> (Size x) {
    uint64_t n = x.rep();
    return n >= w? UInt(0) : UInt(data >> n);
  }


  bool operator == (UInt x) { return rep() == x.rep(); }
  bool operator != (UInt x) { return rep() != x.rep(); }
  bool operator <  (UInt x) { return rep() <  x.rep(); }
  bool operator <= (UInt x) { return rep() <= x.rep(); }
  bool operator >  (UInt x) { return rep() >  x.rep(); }
  bool operator >= (UInt x) { return rep() >= x.rep(); }

  unsigned long asULong()   { return (unsigned long) rep(); }
};


template <size_t a, size_t b>
static inline
UInt<a> lcat(UInt<a> x, UInt<b> y) {
  if constexpr (b >= a) return UInt<a>(y.data);
  return UInt<a>((x.data << b) | y.rep());
}

static inline
int compare(unsigned char rx, unsigned char ry) {
  return (rx < ry) ? -1 : (rx != ry);
}

static inline
int compare(unsigned int rx, unsigned int ry) {
  return (rx < ry) ? -1 : (rx != ry);
}

static inline
int compare(unsigned long rx, unsigned long ry) {
  return (rx < ry) ? -1 : (rx != ry);
}

template <size_t w>
static inline
int compare(UInt<w> x, UInt<w> y) {
  auto rx = x.rep();
  auto ry = y.rep();
  return (rx < ry) ? -1 : (rx != ry);
}


// XXX: Maybe we should consult the base flag, rather than always using hex?
template <size_t w>
static inline
std::ostream& operator<<(std::ostream& os, UInt<w> x) {
  os << "0x" << std::hex;
  os.fill('0');
  if constexpr (w > 0) os.width((w+3)/4);
  os << (uint64_t)x.rep();
  return os;
}


template <size_t w>
static inline
std::ostream& toJS(std::ostream& os, UInt<w> x) {
  return os << std::dec << (uint64_t) x.rep();
}





// Signed ----------------------------------------------------------------------


// XXX: How should arithmetic work on these?
// For the moment we assume no under/overflow, same as C does
// but it is not clear if that's what we want from daedluas.
// XXX: Add `asserts` to detect wrap around in debug mode
template <size_t w>
struct SInt : public Value {
  static_assert(w >= 1,  "SInt needs at least 1 bit");
  static_assert(w <= 64, "SInt larger than 64 not supported.");

  using Rep =
    typename std::conditional < (w <= 8),   int8_t,
    typename std::conditional < (w <= 16),  int16_t,
    typename std::conditional < (w <= 32),  int32_t,
                                            int64_t
    >::type>::type>::type;

  Rep data;

public:
  SInt()      : data(0) {}

  SInt(Rep d) : data(d) {
    if constexpr (std::numeric_limits<Rep>::min() < minValRep())
      assert(minValRep() <= d);
    if constexpr (std::numeric_limits<Rep>::min() > maxValRep())
      assert(d <= maxValRep());
  }

  Rep rep() { return data; } // XXX: overflow?

  SInt operator + (SInt x) { return Rep(data + x.data); }
  SInt operator - (SInt x) { return Rep(data - x.data); }
  SInt operator * (SInt x) { return Rep(data * x.data); }
  SInt operator % (SInt x) { return Rep(data % x.data); }
  SInt operator / (SInt x) { return Rep(data / x.data); }
  SInt operator - ()       { return Rep(-data); }

  bool operator == (SInt<w> x)   { return rep() == x.rep(); }
  bool operator != (SInt<w> x)   { return rep() != x.rep(); }
  bool operator <  (SInt<w> x)   { return rep() <  x.rep(); }
  bool operator >  (SInt<w> x)   { return rep() >  x.rep(); }
  bool operator <= (SInt<w> x)   { return rep() <= x.rep(); }
  bool operator >= (SInt<w> x)   { return rep() >= x.rep(); }

  constexpr static Rep maxValRep() {
    if constexpr (w ==  8) return INT8_MAX;  else
    if constexpr (w == 16) return INT16_MAX; else
    if constexpr (w == 32) return INT32_MAX; else
    if constexpr (w == 64) return INT64_MAX; else
    return (1 << (w-1)) - 1;
  }

  constexpr static Rep minValRep() {
    return (-maxValRep())-1;
  }

  SInt operator << (UInt<64> x) {
    uint64_t n = x.rep();
    return n >= w? SInt(0) : SInt(data << n);
  }

  SInt operator >> (UInt<64> x) {
    uint64_t n = x.rep();
    return n >= w? SInt(0) : SInt(data >> n);
  }

  SInt operator << (Size x) {
    size_t n = x.rep();
    return n >= w? SInt(0) : SInt(data << n);
  }

  SInt operator >> (Size x) {
    size_t n = x.rep();
    return n >= w? SInt(0) : SInt(data >> n);
  }



  // XXX: checks?
  unsigned long asULong() { return (unsigned long) rep(); }
};


/// XXX: What are these for?
static inline
int compare(char rx, char ry) { return (rx < ry) ? -1 : (rx != ry); }
static inline
int compare(int rx,  int ry)  { return (rx < ry) ? -1 : (rx != ry); }
static inline
int compare(long rx, long ry) { return (rx < ry) ? -1 : (rx != ry); }

template <size_t w>
static inline
int compare(SInt<w> x, SInt<w> y) {
  auto rx = x.rep();
  auto ry = y.rep();
  return (rx < ry) ? -1 : (rx != ry);
}


template <size_t a, size_t b>
static inline
SInt<a> lcat(SInt<a> x, UInt<b> y) {
  return (x << b) | y.rep();
}


template <size_t w>
static inline
std::ostream& operator<<(std::ostream& os, SInt<w> x) {
  return os << (int64_t) x.rep();
}

template <size_t w>
static inline
std::ostream& toJS(std::ostream& os, SInt<w> x) {
  return os << (int64_t)x.rep();
}


// -----------------------------------------------------------------------------




#ifdef QUICK_INTEGER
template <size_t b>
static inline
Integer lcat(Integer x, UInt<b> y) {
  return Integer((x.asSLong() << b) | y.rep());
}
#else
template <size_t b>
Integer lcat(Integer x, UInt<b> y) {
  if (x.refCount() == 1) {
    x.mutShiftL(b);
    static_assert(b <= sizeof(unsigned long) * 8);
    x.mutOr(y.rep());
    return x;
  } else {
    mpz_class i = x.getValue() << b;
    Integer r{i};
    r.mutOr(y.rep());
    return r;
  }
}
#endif




} // namespace DDL

#endif

