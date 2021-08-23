#include <iostream>
#include <limits>
#include <ddl/integer.h>

template <typename rep>
void test_con_val(const char *name, rep x) {
  DDL::Integer i = DDL::Integer{x};
  std::cout << name << ": rep = " << x << ", integer = " << i << std::endl;
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

  uint16_t x = -8;
  std::cout << x << std::endl;
}
