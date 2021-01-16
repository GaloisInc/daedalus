#ifndef DDL_BOOL_H
#define DDL_BOOL_H

#include <iostream>

namespace DDL {
class Bool {
  bool b;
public:
  Bool()              {}
  Bool(bool x) : b(x) {}

  bool getValue() { return b; }

  bool operator == (Bool x) { return getValue() == x.getValue(); }
  bool operator != (Bool x) { return getValue() != x.getValue(); }
  bool operator <  (Bool x) { return getValue() <  x.getValue(); }
  bool operator <= (Bool x) { return getValue() <= x.getValue(); }

  Bool operator ! ()        { return Bool(!getValue()); }

};

inline int compare(Bool x, Bool y) { return x.getValue() - y.getValue(); }

inline
std::ostream& operator<<(std::ostream& os, Bool x) {
  os << (x.getValue() ? "true" : "false");
  return os;
}

inline
std::ostream& toJS(std::ostream& os, Bool x) {
  os << (x.getValue() ? "true" : "false");
  return os;
}



}


#endif
