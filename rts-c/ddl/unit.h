#ifndef DDL_UNIT_H
#define DDL_UNIT_H

#include <iostream>
#include <ddl/value.h>

namespace DDL {
class Unit : public Value {};

inline
std::ostream& operator<<(std::ostream& os, Unit x) {
  return os << "{}";
}

inline
std::ostream& toJS(std::ostream& os, Unit x) {
  return os << "{}";
}

inline
int compare(Unit x, Unit y) { return 0; }

}

#endif
