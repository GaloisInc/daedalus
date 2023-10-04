#include <iostream>
#include <fstream>
#include "vlq_base128_le.h"

int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cerr << "Need a file\n";
    return 1;
  }

  std::ifstream file(argv[1], std::ifstream::binary);
  kaitai::kstream ks(&file);
  uint64_t sum = 0;
  while (!ks.is_eof()) {
    vlq_base128_le_t n(&ks);
    sum += n.value();
  }
  std::cout << sum << std::endl;
  return 0;
}
