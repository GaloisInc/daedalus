#ifndef DDL_MAYBE_H
#define DDL_MAYBE_H

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

};

template <typename T>
inline int compare(Maybe<T> x, Maybe<T> y) {
  if (x.isNothing()) return y.isNothing() ? 0 : -1;
  return y.isNothing() ? 1 : compare(x.borrowValue(),y.borrowValue());
}


template <typename T>
inline bool operator == (Maybe<T> x, Maybe<T> y) { return compare(x,y) == 0; }

template <typename T>
inline bool operator != (Maybe<T> x, Maybe<T> y) { return compare(x,y) != 0; }

template <typename T>
inline bool operator < (Maybe<T> x, Maybe<T> y) { return compare(x,y) < 0; }

template <typename T>
inline bool operator > (Maybe<T> x, Maybe<T> y) { return compare(x,y) > 0; }

template <typename T>
inline bool operator <= (Maybe<T> x, Maybe<T> y) { return compare(x,y) <= 0; }

template <typename T>
inline bool operator >= (Maybe<T> x, Maybe<T> y) { return compare(x,y) >= 0; }





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


template <typename T>
inline
std::ostream& toJS(std::ostream& os, Maybe<T> x) {
  if (x.isJust()) {
    os << " { \"$$just\": ";
    toJS(os, x.borrowValue());
    os << "}";
  } else {
    os << "null";
  }
  return os;
}






}

#endif
