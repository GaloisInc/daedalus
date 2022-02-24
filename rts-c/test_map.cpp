#include <ddl/size.h>
#include <ddl/array.h>

void test_map() {

  using El = DDL::Array<DDL::UInt<64>>;

  El a{DDL::Size(0)};
  std::cout << a << std::endl;

  DDL::Map<DDL::UInt<64>,El> x;
  std::cout << x << std::endl;


  auto y = x.insert(1,a);;
  std::cout << y << std::endl;

  El b{DDL::Size(1),7};
  auto z = y.insert(1,b);;
  std::cout << z << std::endl;

  z.free();
}
