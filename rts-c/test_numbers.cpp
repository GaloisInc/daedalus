#include <iostream>
#include <cstdint>
#include <ddl/number.h>

void test_numbers() {
  int8_t v = INT8_C(-6);
  DDL::SInt<3> x{v};
  std::cout << x << std::endl;

  DDL::UInt<8> y {DDL::UInt<4>{UINT8_C(1)}, DDL::UInt<4>{UINT8_C(2)}};
  std::cout << y << std::endl;
}
