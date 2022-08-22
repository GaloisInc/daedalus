#include <iostream>
#include <chrono>
#include "main_parser.h"
#include "../../utils/mainWrapper.cpp"

int go(DDL::Input i) {
  DDL::ParseError err;
  std::vector<DDL::ResultOf::parseMain> out;
  UserState state;
  parseMain(state,err,out,i);
  size_t resultNum = out.size();
  if (resultNum == 0) {
    cout << err;
    return 1;
  } else {
    for (size_t i = 0; i < resultNum; ++i) {
      cout << (i > 0 ? ", " : "[ ");
      DDL::toJS(cout,(DDL::ResultOf::parseMain)out[i]);
      if constexpr (DDL::hasRefs<DDL::ResultOf::parseMain>()) out[i].free();
    }

    cout << "]" << endl;
    return 0;
  }
}

int main(int argc, char* argv[]) { return mkMain(argc,argv,go); }
