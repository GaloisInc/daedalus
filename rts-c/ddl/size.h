#ifndef DDL_SIZE_H
#define DDL_SIZE_H

#include <cstdint>
#include <cassert>
#include <limits>
#include <iostream>


namespace DDL {

// This is so we get a type error rather than an implicit cast.
struct Size {
  size_t value;
public:

  Size(uint8_t x) : value(static_cast<size_t>(x)) {
    if constexpr (std::numeric_limits<uint8_t>::max() >
                  std::numeric_limits<size_t>::max())
      assert(x <= std::numeric_limits<size_t>::max());
  }

  Size(uint16_t x) : value(static_cast<size_t>(x)) {
    if constexpr (std::numeric_limits<uint16_t>::max() >
                  std::numeric_limits<size_t>::max())
      assert(x <= std::numeric_limits<size_t>::max());
  }

  Size(uint32_t x) : value(static_cast<size_t>(x)) {
    if constexpr (std::numeric_limits<uint32_t>::max() >
                  std::numeric_limits<size_t>::max())
      assert(x <= std::numeric_limits<size_t>::max());
  }

  Size(uint64_t x) : value(static_cast<size_t>(x)) {
    if constexpr (std::numeric_limits<uint64_t>::max() >
                  std::numeric_limits<size_t>::max())
      assert(x <= std::numeric_limits<size_t>::max());
  }

  Size(int8_t x) : value(static_cast<size_t>(x)) {
    assert(0 <= x && static_cast<uint8_t>(x) <=
                     std::numeric_limits<size_t>::max());
  }

  Size(int16_t x) : value(static_cast<size_t>(x)) {
    assert(0 <= x && static_cast<uint16_t>(x) <=
                     std::numeric_limits<size_t>::max());
  }

  Size(int32_t x) : value(static_cast<size_t>(x)) {
    assert(0 <= x && static_cast<uint32_t>(x) <=
                     std::numeric_limits<size_t>::max());
  }

  Size(int64_t x) : value(static_cast<size_t>(x)) {
    assert(0 <= x && static_cast<uint64_t>(x) <=
                     std::numeric_limits<size_t>::max());
  }

  size_t rep() { return value; }

  constexpr static size_t maxValRep() {
    return std::numeric_limits<size_t>::max();
  }

  bool operator == (Size x) { return rep() == x.rep(); }
  bool operator != (Size x) { return rep() != x.rep(); }
  bool operator <  (Size x) { return rep() <  x.rep(); }
  bool operator <= (Size x) { return rep() <= x.rep(); }
  bool operator >  (Size x) { return rep() >  x.rep(); }
  bool operator >= (Size x) { return rep() >= x.rep(); }
};

static inline
std::ostream& operator<<(std::ostream& os, Size x) {
  return os << x.rep();
}

static inline
std::ostream& toJS(std::ostream& os, Size x) {
  return os << x;
}

static inline
int compare(Size x, Size y) {
  return x < y ? -1 : x != y;
}

}

#endif

