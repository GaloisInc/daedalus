#include <iostream>
#include <cstdint>
#include <ddl/number.h>

void test_numbers() {
  std::cout << "--- test_numbers ---" << std::endl;
  int8_t v = INT8_C(-6);
  DDL::SInt<3> x{v};
  std::cout << x << std::endl;

  DDL::UInt<8> y {DDL::UInt<4>{UINT8_C(1)}, DDL::UInt<4>{UINT8_C(2)}};
  std::cout << y << std::endl;

  DDL::UInt<8> bits(0xFF);
  DDL::SInt<8> value = DDL::SInt<8>::fromBits(bits);
  std::cout << "bits = " << bits << ", value = " << value << std::endl;

  std::cout << "---------------------" << std::endl;
}
