#include <iostream>
#include <ddl/input.h>
#include <ddl/parse_error.h>
#include "main_parser.h"
#include "exporter.h"

int main() {
  // uint8=1, uint16=2(LE), uint32=3(LE), uint64=4(LE),
  // sint32=-5(LE), sint64=-6(LE), sint8=-7, sint16=-8(LE)
  const unsigned char data[] = {
    1,                              // uint8 = 1
    2, 0,                           // uint16 = 2
    3, 0, 0, 0,                     // uint32 = 3
    4, 0, 0, 0, 0, 0, 0, 0,        // uint64 = 4
    0xFB, 0xFF, 0xFF, 0xFF,         // sint32 = -5
    0xFA, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, // sint64 = -6
    0xF9,                           // sint8 = -7
    0xF8, 0xFF                      // sint16 = -8
  };
  DDL::Input input("test", (const char*)data, DDL::Size(sizeof(data)));
  DDL::ParseError<DDL::Input> err;
  std::vector<User::Main> results;
  parseMain(err, results, input);
  if (results.empty()) { std::cerr << "parse failed\n"; return 1; }
  std::cout << spec::exportMain(results[0]) << std::endl;
  return 0;
}
