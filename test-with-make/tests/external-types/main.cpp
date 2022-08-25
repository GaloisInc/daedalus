#include <iostream>
#include <ddl/utils.h>
#include "../../utils/mainWrapper.cpp"
#include "P1/main_parser.h"
#include "P2/main_parser.h"

int go(DDL::Input i) {
  DDL::ParseError err;
  User::P1 result;
  if (parseOne(parseP2,err,&result,i)) {
    std::cout << result;
    return 0;
  } else {
    std::cout << err;
    return 1;
  }
}

int main(int argc, char* argv[]) { return mkMain(argc,argv,go); }
