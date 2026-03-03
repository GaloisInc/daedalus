#include <iostream>
#include <ddl/input.h>
#include <ddl/parse_error.h>
#include "main_parser.h"
#include "exporter.h"

int main() {
  // LE uint32 = 42 (parsed as just value of maybe)
  const unsigned char data[] = {42, 0, 0, 0};
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
