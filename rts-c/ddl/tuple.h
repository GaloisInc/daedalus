#ifndef DDL_TUPLE_H
#define DDL_TUPLE_H

#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>

#include <ddl/boxed.h>

namespace DDL {

template <typename... Ts>
class Tuple : public HasRefs {
  std::tuple<Ts...> values;

  template <std::size_t... Is>
  void copyElements(std::index_sequence<Is...>) {
    (copyElement<Is>(), ...);
  }

  template <std::size_t i>
  void copyElement() {
    std::get<i>(values).copy();
  }

  template <std::size_t... Is>
  void freeElements(std::index_sequence<Is...>) {
    (freeElement<Is>(), ...);
  }

  template <std::size_t i>
  void freeElement() {
    std::get<i>(values).free();
  }

public:
  Tuple() = default;
  template <
    typename... Us,
    std::enable_if_t<(sizeof...(Us) > 0), int> = 0>
  Tuple(Us&&... xs) : values(std::forward<Us>(xs)...) {}

  // borrow this, borrow result
  template <std::size_t i>
  auto borrow() {
    return std::get<i>(values);
  }

  // borrow this, own result
  template <std::size_t i>
  auto get() {
    using T = std::tuple_element_t<i, std::tuple<Ts...>>;
    T result = std::get<i>(values);
    result.copy();
    return result;
  }

  void copy() {
    copyElements(std::index_sequence_for<Ts...>{});
  }

  void free() {
    freeElements(std::index_sequence_for<Ts...>{});
  }
};


template <std::size_t i, typename... Ts>
int compareTuple(Tuple<Ts...> x, Tuple<Ts...> y) {
  if constexpr (i == sizeof...(Ts)) {
    return 0;
  } else {
    int result = compare(x.template borrow<i>(), y.template borrow<i>());
    return result == 0 ? compareTuple<i + 1>(x, y) : result;
  }
}

template <typename... Ts>
inline int compare(Tuple<Ts...> x, Tuple<Ts...> y) {
  return compareTuple<0>(x, y);
}

template <typename... Ts>
inline bool operator==(Tuple<Ts...> x, Tuple<Ts...> y) {
  return compare(x, y) == 0;
}

template <typename... Ts>
inline bool operator!=(Tuple<Ts...> x, Tuple<Ts...> y) {
  return compare(x, y) != 0;
}

template <typename... Ts>
inline bool operator<(Tuple<Ts...> x, Tuple<Ts...> y) {
  return compare(x, y) < 0;
}

template <typename... Ts>
inline bool operator<=(Tuple<Ts...> x, Tuple<Ts...> y) {
  return compare(x, y) <= 0;
}

template <typename... Ts>
inline bool operator>(Tuple<Ts...> x, Tuple<Ts...> y) {
  return compare(x, y) > 0;
}

template <typename... Ts>
inline bool operator>=(Tuple<Ts...> x, Tuple<Ts...> y) {
  return compare(x, y) >= 0;
}


template <typename... Ts, std::size_t... Is>
std::ostream& printTuple(std::ostream& os, Tuple<Ts...> x,
                         std::index_sequence<Is...>) {
  std::size_t n = 0;
  os << "(";
  ((os << (n++ == 0 ? "" : ", ") << x.template borrow<Is>()), ...);
  return os << ")";
}

template <typename... Ts>
inline std::ostream& operator<<(std::ostream& os, Tuple<Ts...> x) {
  return printTuple(os, x, std::index_sequence_for<Ts...>{});
}


template <typename... Ts, std::size_t... Is>
std::ostream& tupleToJS(std::ostream& os, Tuple<Ts...> x,
                        std::index_sequence<Is...>) {
  std::size_t n = 0;
  os << "{ \"$$tuple\": [";
  ((os << (n++ == 0 ? "" : ", "),
    toJS(os, x.template borrow<Is>())), ...);
  return os << "] }";
}

template <typename... Ts>
inline std::ostream& toJS(std::ostream& os, Tuple<Ts...> x) {
  return tupleToJS(os, x, std::index_sequence_for<Ts...>{});
}

}

#endif
