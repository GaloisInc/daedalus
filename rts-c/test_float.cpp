#include <ddl/float.h>

void test_float() {
  DDL::Float x(0.5);
  std::cout << x << ", isnan = " << x.isNaN() << std::endl;
}
