#include <iostream>
#include <ddl/number.h>

void test_numbers() {
  int8_t v = INT8_C(-6);
  DDL::SInt<3> x(v);
  std::cout << x << std::endl;
}
