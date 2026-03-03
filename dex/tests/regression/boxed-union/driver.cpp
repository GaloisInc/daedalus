#include <iostream>
#include <ddl/input.h>
#include <ddl/parse_error.h>
#include "main_parser.h"
#include "exporter.h"

int main() {
  // cons(1, cons(2, nil)): byte 1 (cons, val=1), byte 2 (cons, val=2), byte 0 (nil)
  const unsigned char data[] = {1, 2, 0};
  DDL::Input input("test", (const char*)data, DDL::Size(sizeof(data)));
  DDL::ParseError<DDL::Input> err;
  std::vector<User::IntList> results;
  parseMain(err, results, input);
  if (results.empty()) { std::cerr << "parse failed\n"; return 1; }
  for (auto result : results) {
    std::cout << spec::exportMain(result) << std::endl;
  }
  return 0;
}
