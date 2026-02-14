#include <iostream>
#include <ddl/input.h>
#include <ddl/parse_error.h>
#include "main_parser.h"
#include "exporter.h"

int main() {
  // n=2 outer arrays
  // inner[0]: len=3, bytes=[1,2,3] -> sum=6
  // inner[1]: len=2, bytes=[4,5]   -> sum=9
  const unsigned char data[] = {2, 3, 1, 2, 3, 2, 4, 5};
  DDL::Input input("test", (const char*)data, DDL::Size(sizeof(data)));
  DDL::ParseError<DDL::Input> err;
  std::vector<User::Main> results;
  parseMain(err, results, input);
  if (results.empty()) { std::cerr << "parse failed\n"; return 1; }
  std::cout << spec::exportMain(results[0]) << std::endl;
  return 0;
}
