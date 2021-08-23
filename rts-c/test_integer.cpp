#include <iostream>
#include <limits>
#include <ddl/integer.h>

template <typename rep>
void test_con_val(const char *name, rep x) {
  DDL::Integer i = DDL::Integer(x);
  rep back = 17, big = 17, small = 17;
  i.exportI(back);
  uint64_t v = 3;
  DDL::Integer x3 = DDL::Integer(v);
  i.copy();
  x3.copy();
  DDL::Integer j = i - x3;
  DDL::Integer k = i + x3;
  k.exportI(big);
  j.exportI(small);
  std::cout << "  " << name << ":\n    rep = " << x << "\n    integer = " << i
                                  << "\n    back = " << back
      << "\n    big = " << big << "\n    small = " << small
      << std::endl;
  j.free();
  k.free();
}

template <typename rep>
void test_con(const char *name) {
  std::cout << name << std::endl;
  test_con_val<rep>("min", std::numeric_limits<rep>::min());
  test_con_val<rep>("7", 7);
  test_con_val<rep>("0  ", 0);
  test_con_val<rep>("-7", -7);
  test_con_val<rep>("max", std::numeric_limits<rep>::max());
}


void test_integer() {
  test_con<uint8_t>("u8");
  test_con<uint16_t>("u16");
  test_con<uint32_t>("u32");
  test_con<uint64_t>("u64");
  test_con<int8_t>("s8");
  test_con<int16_t>("s16");
  test_con<int32_t>("s32");
  test_con<int64_t>("s64");
}
