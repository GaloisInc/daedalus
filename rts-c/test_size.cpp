
#include <ddl/size.h>
#include <iostream>

void test_size() {
  uint8_t  x8 = std::numeric_limits<uint8_t>::max();
  uint16_t x16 = std::numeric_limits<uint16_t>::max();
  uint32_t x32 = std::numeric_limits<uint32_t>::max();
  uint64_t x64 = std::numeric_limits<uint64_t>::max();

  std::cout << Size(x8) << " "
            << Size(x16) << " "
            << Size(x32) << " "
            << Size(x64) << "\n";
}
