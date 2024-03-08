#pragma once

#include <cmath>
#include <string_view>
#include <iostream>

namespace DDL {

inline
std::ostream& toJS(std::ostream& os, double x) {
  if (std::isnan(x)) return os << "\"NAN\"";
  if (std::isinf(x)) return os << "\"" << (x > 0 ? "+" : "-") << "inf\"";
  return os << x;
}

inline
std::ostream& toJS(std::ostream& os, float x) {
  return toJS(os, (double) x);
}


static inline
std::ostream& toJS(std::ostream &os, std::string_view const& str) {
  const char hex[] = "0123456789abcdef";
  os << "\"";
  for (auto&& c : str) {
    switch (c) {
      case '"':  os << "\\\""; break;
      case '\b': os << "\\b"; break;
      case '\f': os << "\\f"; break;
      case '\n': os << "\\n"; break;
      case '\r': os << "\\r"; break;
      case '\t': os << "\\t"; break;
      case '\\': os << "\\\\"; break;
      default:
        if (32 <= c && c < 127) {
          os << c;
        } else {
          auto v = c;
          auto d1 = hex[v % 16]; v /= 16;
          auto d2 = hex[v % 16]; v /= 16;
          auto d3 = hex[v % 16]; v /= 16;
          auto d4 = hex[v % 16];
          os << "\\u" << d4 << d3 << d2 << d1;
        }
    }
  }
  os << "\"";
  return os;
}



template <typename T>
struct JS {
  T x;
  JS(T x) : x(x) {};
};

template<typename T>
std::ostream &operator << (std::ostream &os, JS<T> x) {
  return toJS(os,x.x);
}

}

