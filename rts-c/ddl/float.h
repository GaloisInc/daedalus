#ifndef DDL_FLOAT_H
#define DDL_FLOAT_H

#include <iostream>
#include <cmath>
#include <cstring>

#include <ddl/value.h>

namespace DDL {

class Float : public Value {
  float f;
public:
  Float() {}
  Float(float x) : f(x) {}

  static Float fromBits(uint32_t x) {
    float f;
    static_assert(sizeof(x) == sizeof(f));
    std::memcpy(&f, &x, sizeof(x));
    return Float{f};
  }

  float getValue() { return f; }

  bool operator == (Float x) { return getValue() == x.getValue(); }
  bool operator != (Float x) { return getValue() != x.getValue(); }
  bool operator <  (Float x) { return getValue() <  x.getValue(); }
  bool operator <= (Float x) { return getValue() <= x.getValue(); }

  bool isNaN() { return std::isnan(getValue()); }
  bool isInfinite() { return std::isinf(getValue()); }
  bool isDenormalized() { return std::fpclassify(f) == FP_SUBNORMAL; }
  bool isNegativeZero() { return f == 0 && std::signbit(f); }
};

inline int compare(Float x, Float y) {
  auto a = x.getValue();
  auto b = y.getValue();
  return (a > b) ? 1 : (a < b) ? (-1) : 0;
}

inline
std::ostream& operator<<(std::ostream& os, Float x) {
  os << x.getValue();
  return os;
}

inline
std::ostream& toJS(std::ostream& os, Float x) {
  os << x.getValue();
  return os;
}


// -----------------------------------------------------------------------------
class Double : public Value {
  double f;
public:
  Double() {}
  Double(double x) : f(x) {}

  static Double fromBits(uint64_t x) {
    double f;
    static_assert(sizeof(x) == sizeof(f));
    std::memcpy(&f, &x, sizeof(x));
    return Double{f};
  }

  double getValue() { return f; }

  bool operator == (Double x) { return getValue() == x.getValue(); }
  bool operator != (Double x) { return getValue() != x.getValue(); }
  bool operator <  (Double x) { return getValue() <  x.getValue(); }
  bool operator <= (Double x) { return getValue() <= x.getValue(); }

  bool isNaN() { return std::isnan(getValue()); }
  bool isInfinite() { return std::isinf(getValue()); }
  bool isDenormalized() { return std::fpclassify(f) == FP_SUBNORMAL; }
  bool isNegativeZero() { return f == 0 && std::signbit(f); }
};

inline int compare(Double x, Double y) {
  auto a = x.getValue();
  auto b = y.getValue();
  return (a > b) ? 1 : (a < b) ? (-1) : 0;
}

inline
std::ostream& operator<<(std::ostream& os, Double x) {
  os << x.getValue();
  return os;
}

inline
std::ostream& toJS(std::ostream& os, Double x) {
  os << x.getValue();
  return os;
}



}


#endif
