#include <ddl/array.h>



void test_array() {
  DDL::Array<DDL::UInt<8>> x(5,'x', 'a','b','c',1);
  std::cout << x << std::endl;
  x.free();
}
