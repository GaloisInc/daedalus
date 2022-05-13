#ifndef DDL_UNIT_H
#define DDL_UNIT_H

#include <iostream>
#include <ddl/size.h>
#include <ddl/value.h>
#include <ddl/number.h>

namespace DDL {
struct Unit : public Value {

  static constexpr Width bitWidth = 0;

  UInt<bitWidth> toBits()                { return UInt<bitWidth>(0); }
  static Unit fromBits(UInt<bitWidth> x) { return Unit{}; }
  static bool isValid(UInt<bitWidth> x)  { return true; }
};

inline bool operator==(Unit, Unit) { return true; }
inline bool operator!=(Unit, Unit) { return false; }
inline bool operator<=(Unit, Unit) { return true; }
inline bool operator>=(Unit, Unit) { return true; }
inline bool operator<(Unit, Unit) { return false; }
inline bool operator>(Unit, Unit) { return false; }

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
