#include <ddl/float.h>

void test_float() {
  DDL::Float x(0.5);
  std::cout << x << ", isnan = " << x.isNaN() << std::endl;

  DDL::Float z{0};
  DDL::Float a = z/z;
  std::cout << a << ", isnan = " << a.isNaN() << std::endl;
}
