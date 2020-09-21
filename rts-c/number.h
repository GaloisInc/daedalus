#ifndef DDL_NUMBER
#define DDL_NUMBER

#include <cstdint>
#include <type_traits>
#include <iostream>
#include <ios>
#include <cmath>

namespace DDL {

using Integer = int; // XXX: temporary

template <int w>
struct UInt {
  static_assert(w <= 64);   // XXX: use gmp?

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
  UInt<w> operator + (UInt<w> x) { return { .data = Rep(data + x.data) }; }
  UInt<w> operator - (UInt<w> x) { return { .data = Rep(data - x.data) }; }
  UInt<w> operator * (UInt<w> x) { return { .data = Rep(data * x.data) }; }
  UInt<w> operator / (UInt<w> x) { return { .data = Rep(rep() / x.rep()) }; }
  UInt<w> operator - ()          { return { .data = Rep(-data) }; }

  bool operator == (UInt<w> x) { return rep() == x.rep(); }
  bool operator != (UInt<w> x) { return rep() != x.rep(); }

  Rep rep() {
    if constexpr (w == 8 || w == 16 || w == 32 || w == 64) return data;
    if constexpr (w < 8)   return data & (UINT8_MAX >> (8-w));
    if constexpr (w < 16)  return data & (UINT16_MAX >> (16-w));
    if constexpr (w < 32)  return data & (UINT32_MAX >> (32-w));
    return data;
  }
};

}

// XXX: Maybe we should consult the base flag, rather than always using hex?
template <int w>
static inline
std::ostream& operator<<(std::ostream& os, DDL::UInt<w> x) {
  std::ios_base::fmtflags saved(os.flags());

  os << "0x" << std::hex;
  os.fill('0');
  if constexpr (w > 0) os.width((w+3)/4);
  os << (uint64_t)x.rep();
  os.flags(saved);
  return os;
}

namespace std {
  template<int w>
  struct hash<DDL::UInt<w>> {
    std::size_t operator()(DDL::UInt<w> x) const noexcept {
      return size_t(x.data);
    }
  };
}

namespace DDL {


// XXX: Show should arithmetic work on these?
// For the moment we assume no under/overflow, same as C does
// but it is not clear if that's what we want from daedluas.
template <int w>
struct SInt {
  static_assert(w <= 64);   // XXX: use gmp?

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
  SInt<w> operator + (SInt<w> x) { return { .data = Rep(data + x.data) }; }
  SInt<w> operator - (SInt<w> x) { return { .data = Rep(data - x.data) }; }
  SInt<w> operator * (SInt<w> x) { return { .data = Rep(data * x.data) }; }
  SInt<w> operator / (SInt<w> x) { return { .data = Rep(data / x.data) }; }
  SInt<w> operator - ()          { return { .data = Rep(-data) }; }

  bool operator == (SInt<w> x) { return rep() == x.rep(); }
  bool operator != (SInt<w> x) { return rep() != x.rep(); }

  Rep rep() { return data; }
};

}

// XXX: Maybe we should consult the base flag, rather than always using hex?
template <int w>
static inline
std::ostream& operator<<(std::ostream& os, DDL::SInt<w> x) {
  os << (int64_t) x.rep();
  return os;
}

namespace std {
  template<int w>
  struct hash<DDL::SInt<w>> {
    std::size_t operator()(DDL::SInt<w> x) const noexcept {
      return size_t(x.data);
    }
  };
}


#endif

