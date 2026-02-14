#include <iostream>
#include <ddl/input.h>
#include <ddl/parse_error.h>
#include "main_parser.h"
#include "exporter.h"

int main() {
  // IEEE 754 LE: float 2.5 = 0x40200000, double 3.75 = 0x400E000000000000
  const unsigned char data[] = {
    0x00, 0x00, 0x20, 0x40,                          // float 2.5
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0E, 0x40   // double 3.75
  };
  DDL::Input input("test", (const char*)data, DDL::Size(sizeof(data)));
  DDL::ParseError<DDL::Input> err;
  std::vector<User::Main> results;
  parseMain(err, results, input);
  if (results.empty()) { std::cerr << "parse failed\n"; return 1; }
  std::cout << spec::exportMain(results[0]) << std::endl;
  return 0;
}
