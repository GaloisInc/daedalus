#ifndef DDL_SIZE_H
#define DDL_SIZE_H

#include <cstdint>
#include <cassert>
#include <limits>
#include <iostream>


namespace DDL {

struct SizeOverflow {};
struct SizeUnderflow {};


class Size;
typedef Size RefCount;    // Used for counting references
typedef size_t Width;       // Used for type parameters


const size_t max_size_t = std::numeric_limits<size_t>::max();

// This is so we get a type error rather than an implicit cast.
class Size {
  size_t value;
  size_t remaining() const { return max_size_t - value; }

public:

  Size() : value(0) {}
  Size(size_t x) : value(x) {}

  template <typename T>
  static Size from(T x) {
    if (0 <= x && x <= max_size_t) {
      return Size{static_cast<size_t>(x)};
    } else {
      throw SizeOverflow{};
    }

  }

  size_t rep() const { return value; }

  static Size maxValue() { return Size{max_size_t}; }

  bool operator == (Size x) const { return rep() == x.rep(); }
  bool operator != (Size x) const { return rep() != x.rep(); }
  bool operator <  (Size x) const { return rep() <  x.rep(); }
  bool operator <= (Size x) const { return rep() <= x.rep(); }
  bool operator >  (Size x) const { return rep() >  x.rep(); }
  bool operator >= (Size x) const { return rep() >= x.rep(); }

  // mutatations
  void incrementBy(Size x) {
    size_t n = x.rep();
    if (n <= remaining()) {
      value += n;
    } else {
      throw SizeOverflow();
    }
  }
  void decrementBy(Size x) {
    if (x > *this) throw SizeUnderflow();
    value -= x.rep();
  }
  void increment() { incrementBy(Size(1)); }
  void decrement() { decrementBy(Size(1)); }

  void scaleBy(Size x) {
    if (value == 0) return;
    size_t have = max_size_t / value;
    auto n = x.rep();
    if (n > have) throw SizeOverflow{};
    value *= n;
  }

  void inUnitsOf(Size x) {
    auto n = x.rep();
    if (n == 0) throw SizeOverflow();
    auto r = value / n;
    value = value % n? r + 1 : r;
  }

  // immutable
  Size incrementedBy(Size x) const { Size y(value); y.incrementBy(x); return y;}
  Size decrementedBy(Size x) const { Size y(value); y.decrementBy(x); return y;}
  Size incremented()         const { return incrementedBy(Size(1)); }
  Size decremented()         const { return decrementedBy(Size(1)); }
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

