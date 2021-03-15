#ifndef DDL_NUMBER_H
#define DDL_NUMBER_H

#include <cstdint>
#include <type_traits>
#include <iostream>
#include <ios>
#include <cmath>

#include <ddl/bool.h>
#include <ddl/integer.h>


namespace DDL {

template <int w>
struct UInt {
  static_assert(w <= 64, "UInt larger than 64 not supported.");

  using Rep =
    typename std::conditional < (w <= 8),   uint8_t,
    typename std::conditional < (w <= 16),  uint16_t,
    typename std::conditional < (w <= 32),  uint32_t,
    typename std::conditional < (w <= 64),  uint64_t,
                                            uint64_t    // XXX: gmp
    >::type>::type>::type>::type;

  Rep data;

public:
  UInt() : data(0) {}
  UInt(Rep d) : data(d) {}

  Rep rep() {
    if constexpr (w == 8 || w == 16 || w == 32 || w == 64) return data;
    if constexpr (w < 8)   return data & (UINT8_MAX >> (8-w));
    if constexpr (w < 16)  return data & (UINT16_MAX >> (16-w));
    if constexpr (w < 32)  return data & (UINT32_MAX >> (32-w));
    return data;
  }

  constexpr static Rep maxValRep() {
    if constexpr (w ==  8) return UINT8_MAX;  else
    if constexpr (w == 16) return UINT16_MAX; else
    if constexpr (w == 32) return UINT32_MAX; else
    if constexpr (w == 64) return UINT64_MAX; else
    return (1 << w) - 1;
  }


  // This is for `a # b`
  template <int a, int b>
  UInt(UInt<a> x, UInt<b> y) : data((Rep(x.data) << b) | y.rep()) {}

  UInt operator + (UInt x) { return UInt(data + x.data); }
  UInt operator - (UInt x) { return UInt(data - x.data); }
  UInt operator * (UInt x) { return UInt(data * x.data); }
  UInt operator % (UInt x) { return UInt(rep() % x.rep()); }
  UInt operator / (UInt x) { return UInt(rep() / x.rep()); }
  UInt operator - ()       { return UInt(-data); }
  UInt operator ~ ()       { return UInt(~data); }

  UInt operator | (UInt x) { return UInt(data | x.data); }
  UInt operator & (UInt x) { return UInt(data & x.data); }
  UInt operator ^ (UInt x) { return UInt(data ^ x.data); }

  // yikes, we really should use something other than integer here
  // we are borrowing the integer
  UInt operator << (UInt<64> x) {
    uint64_t n = x.rep();
    return n >= w? UInt(0) : UInt(data << n);
  }

  // same as for <<
  UInt operator >> (UInt<64> x) {
    uint64_t n = x.rep();
    return n >= w? UInt(0) : UInt(data >> n);
  }

  bool operator == (UInt x)   { return rep() == x.rep(); }
  bool operator != (UInt x)   { return rep() != x.rep(); }
  bool operator <  (UInt x)   { return rep() <  x.rep(); }
  bool operator <= (UInt x)   { return rep() <= x.rep(); }
  bool operator >  (UInt<w> x){ return rep() >  x.rep(); }
  bool operator >= (UInt<w> x){ return rep() >=  x.rep(); }

};


template <int a, int b>
static inline
UInt<a> lcat(UInt<a> x, UInt<b> y) {
  if constexpr (b >= a) return UInt<a>(y.data);
  return UInt<a>((x.data << b) | y.rep());
}

#ifdef QUICK_INTEGER
template <int b>
static inline
Integer lcat(Integer x, UInt<b> y) {
  return Integer((x.asSLong() << b) | y.rep());
}
#else
template <int b>
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

template <int w>
static inline
int compare(UInt<w> x, UInt<w> y) {
  auto rx = x.rep();
  auto ry = y.rep();
  return (rx < ry) ? -1 : (rx != ry);
}







// XXX: Maybe we should consult the base flag, rather than always using hex?
template <int w>
static inline
std::ostream& operator<<(std::ostream& os, UInt<w> x) {
  os << "0x" << std::hex;
  os.fill('0');
  if constexpr (w > 0) os.width((w+3)/4);
  os << (uint64_t)x.rep();
  return os;
}


template <int w>
static inline
std::ostream& toJS(std::ostream& os, UInt<w> x) {
  return os << std::dec << (unsigned long) x.rep();
}





// XXX: How should arithmetic work on these?
// For the moment we assume no under/overflow, same as C does
// but it is not clear if that's what we want from daedluas.
template <int w>
struct SInt {
  static_assert(w >= 1, "SInt needs at least 1 bit");
  static_assert(w <= 64, "SInt larger than 64 not supported.");

  using Rep =
    typename std::conditional < (w <= 8),   int8_t,
    typename std::conditional < (w <= 16),  int16_t,
    typename std::conditional < (w <= 32),  int32_t,
    typename std::conditional < (w <= 64),  int64_t,
                                            int64_t    // XXX: gmp
    >::type>::type>::type>::type;

  Rep data;

public:
  SInt() : data(0) {}
  SInt(Rep d) : data(d) {}
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
    return -maxValRep()-1;
  }

  SInt operator << (UInt<64> x) {
    uint64_t n = x.rep();
    return n >= w? SInt(0) : SInt(data << n);
  }

  // same as for <<
  SInt operator >> (UInt<64> x) {
    uint64_t n = x.rep();
    return n >= w? SInt(0) : SInt(data >> n);
  }
};


static inline
int compare(char rx, char ry) { return (rx < ry) ? -1 : (rx != ry); }
static inline
int compare(int rx,  int ry)  { return (rx < ry) ? -1 : (rx != ry); }
static inline
int compare(long rx, long ry) { return (rx < ry) ? -1 : (rx != ry); }

template <int w>
static inline
int compare(SInt<w> x, SInt<w> y) {
  auto rx = x.rep();
  auto ry = y.rep();
  return (rx < ry) ? -1 : (rx != ry);
}



template <int a, int b>
static inline
SInt<a> lcat(SInt<a> x, SInt<b> y) {
  return (x << b) | y.rep();
}


template <int w>
static inline
std::ostream& operator<<(std::ostream& os, SInt<w> x) {
  return os << (int64_t) x.rep();
}

template <int w>
static inline
std::ostream& toJS(std::ostream& os, SInt<w> x) {
  return os << (long)x.rep();
}




}

#endif

