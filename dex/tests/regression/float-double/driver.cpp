#include <iostream>
#include <cstring>
#include <ddl/input.h>
#include <ddl/parse_error.h>
#include "main_parser.h"
#include "exporter.h"

int main() {
  // IEEE 754: float 2.5 = 0x40200000, double 3.75 = 0x400E000000000000
  char data[12];
  float f = 2.5f;
  double d = 3.75;
  std::memcpy(data, &f, 4);
  std::memcpy(data + 4, &d, 8);
  DDL::Input input("test", data, DDL::Size(12));
  DDL::ParseError<DDL::Input> err;
  std::vector<User::Main> results;
  parseMain(err, results, input);
  if (results.empty()) { std::cerr << "parse failed\n"; return 1; }
  std::cout << spec::exportMain(results[0]) << std::endl;
  return 0;
}
