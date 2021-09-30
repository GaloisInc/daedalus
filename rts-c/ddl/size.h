#ifndef DDL_SIZE_H
#define DDL_SIZE_H

#include <cstdint>
#include <cassert>
#include <limits>
#include <iostream>


namespace DDL {

typedef size_t RefCount;    // Used for counting references
typedef size_t Width;       // Used for type parameters

// This is so we get a type error rather than an implicit cast.
struct Size {
  size_t value;
public:

  Size() : value(0) {}
  Size(size_t x) : value(x) {}

  template <typename T>
  static Size from(T x) {
    assert(0 <= x && x <= std::numeric_limits<size_t>::max());
    return Size{static_cast<size_t>(x)};
  }

  size_t rep() const { return value; }

  bool operator == (Size x) const { return rep() == x.rep(); }
  bool operator != (Size x) const { return rep() != x.rep(); }
  bool operator <  (Size x) const { return rep() <  x.rep(); }
  bool operator <= (Size x) const { return rep() <= x.rep(); }
  bool operator >  (Size x) const { return rep() >  x.rep(); }
  bool operator >= (Size x) const { return rep() >= x.rep(); }

  // mutatations
  void incrementBy(Size x) { value += x.rep(); }
  void increment()         { incrementBy(Size{1}); }
  void decrement()         { value -= 1; }

  // immutable
  Size incrementedBy(Size x) const { return Size{value + x.rep()}; }
  Size incremented()         const { return incrementedBy(Size{1}); }
  Size decremented()         const { return Size{value - 1 }; }
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

