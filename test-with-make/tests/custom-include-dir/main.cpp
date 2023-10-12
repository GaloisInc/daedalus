#include "include/custom_parser.h"
#include "../../utils/mainWrapper.cpp"
#include <ddl/utils.h>

int go(DDL::Input i) {
  DDL::ParseError<DDL::Input> error;
  DDL::UInt<8> res;
  auto ok = DDL::parseOne(parseMain, error, &res, i);
  if (ok) {
    std::cout << res << std::endl;
    return 0;
  } else {
    std::cout << error << std::endl;
    return 1;
  }
}

