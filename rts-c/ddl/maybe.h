#ifndef DDL_MAYBE
#define DDD_MAYBE

#include <iostream>
#include <ddl/boxed.h>

namespace DDL {

template <typename T>
class Maybe : HasRefs {
  bool is_just;
  T    value;
public:
  Maybe()    : is_just(false) {}
  Maybe(T v) : is_just(true), value(v) {}

  // borrow this
  bool isJust()     { return is_just; }
  bool isNothing () { return !is_just; }

  // borrow this, owned result
  T    getValue() { if constexpr (hasRefs<T>()) value.copy(); return value; }

  // borrow this, borrow value
  T    borrowValue() { return value; }

  void copy() { if constexpr (hasRefs<T>()) if (isJust()) value.copy(); }
  void free() { if constexpr (hasRefs<T>()) if (isJust()) value.free(); }

  bool operator == (Maybe x) {
    return isJust() ? (x.isJust() && value == x.value)
                    : !x.isJust();
  }

  bool operator != (Maybe x) { return !(*this == x); }

  bool operator < (Maybe x) {
    return isJust() ? (x.isJust() && value < x.value)
                    : x.isJust();
  }

  bool operator <= (Maybe x) {
    return isJust() ? (x.isJust() && value <= x.value)
                    : true;
  }

};




template <typename T>
inline
std::ostream& operator<<(std::ostream& os, Maybe<T> x) {
  if (x.isJust()) {
    os << "just " << x.borrowValue();
  } else {
    os << "nothing";
  }
  return os;
}




}

#endif
