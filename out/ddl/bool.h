#ifndef DDL_BOOL_H
#define DDL_BOOL_H

#include <iostream>

#include <ddl/value.h>

namespace DDL {

class Bool : public Value {
  bool b;
public:
  Bool()              {}
  Bool(bool x) : b(x) {}

  bool getValue() const { return b; }

};

inline bool operator == (Bool x, Bool y) { return x.getValue() == y.getValue(); }
inline bool operator != (Bool x, Bool y) { return x.getValue() != y.getValue(); }
inline bool operator <  (Bool x, Bool y) { return x.getValue() <  y.getValue(); }
inline bool operator <= (Bool x, Bool y) { return x.getValue() <= y.getValue(); }
inline bool operator >  (Bool x, Bool y) { return x.getValue() >  y.getValue(); }
inline bool operator >= (Bool x, Bool y) { return x.getValue() >= y.getValue(); }
inline Bool operator ! (Bool x)  { return Bool(!x.getValue()); }

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
