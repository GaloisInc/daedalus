#ifndef DDL_UNIT_H
#define DDL_UNIT_H

namespace DDL {
class Unit {};
}

namespace std {
  template<>
  struct hash<DDL::Unit> {
    size_t operator()(DDL::Unit x) const noexcept { return 17; }
  };
}
#endif
