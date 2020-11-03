#ifndef DDL_BOOL
#define DDL_BOOL

#include <iostream>

namespace DDL {
class Bool {
  bool b;
public:
  Bool() {}
  Bool(bool x) : b(x) {}
  bool getValue() { return b; }

  bool operator == (Bool x) { return getValue() == x.getValue(); }
  bool operator != (Bool x) { return getValue() != x.getValue(); }
  bool operator <  (Bool x) { return getValue() <  x.getValue(); }
  bool operator <= (Bool x) { return getValue() <= x.getValue(); }

};

inline
std::ostream& operator<<(std::ostream& os, Bool x) {
  os << (x.getValue() ? "true" : "false");
  return os;
}

}


#endif
