#ifndef DDL_UNIT_H
#define DDL_UNIT_H

namespace DDL {
class Unit {};

inline
std::ostream& operator<<(std::ostream& os, Unit x) {
  return os << "{}";
}

}

#endif
