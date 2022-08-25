#include "./utils/mainWrapper.cpp"
#include <ddl/utils.h>
#include "main_parser.h"

int go(DDL::Input i) {
  DDL::ParseError error;
  std::vector<DDL::Array <DDL::UInt<8>>> res;
  int state = 0;
  parseMain(state, error, res, i);

  bool first = true;
  for (auto a : res) {
    std::cout << (first ? "[ " : ", ");
    first = false;
    DDL::toJS(std::cout,a);
    a.free();
  }
  std::cout << "]\n";
  return 0;
}
