#include <iostream>
#include <ddl/input.h>
#include <ddl/parse_error.h>
#include "main_parser.h"
#include "exporter.h"

int main() {
  // len=3, first=[1,2,3] (3 bytes), second=[4,5] (remaining 2 bytes)
  const unsigned char data[] = {3, 1, 2, 3, 4, 5};
  DDL::Input input("test", (const char*)data, DDL::Size(sizeof(data)));
  DDL::ParseError<DDL::Input> err;
  std::vector<User::Main> results;
  parseMain(err, results, input);
  if (results.empty()) { std::cerr << "parse failed\n"; return 1; }
  for (auto result : results) {
    std::cout << spec::exportMain(result) << std::endl;
  }
  return 0;
}
