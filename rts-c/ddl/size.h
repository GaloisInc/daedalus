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

  Size(size_t x) : value(x) {}

  template <typename T>
  static Size from(T x) {
    assert(0 <= x && x <= std::numeric_limits<size_t>::max());
    return Size{static_cast<size_t>(x)};
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

