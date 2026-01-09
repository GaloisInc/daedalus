#include "parser/main_parser.h"
#include <ddl/parser.h>
#include <ddl/number.h>
#include <vector>
#include <iostream>

struct S {
  int x;
  int y;
};


S exportMain(User::Main x) {
  return { .x = int{x.get_x().rep()}, .y = int{x.get_y().rep()} };
}

int main() {
  DDL::ParseError<DDL::Input> error;
  std::vector<User::Main> results;
  parseMain(error, results, DDL::Input());
  for(auto i : results) {
    auto s = exportMain(i);
    std::cout << "x = " << s.x << "; y = " << s.y << "\n";
  }
  if (results.size() == 0) {
    std::cout << "parse error\n";
  }
  
  return 0;
}