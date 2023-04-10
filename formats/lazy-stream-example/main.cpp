#include <iostream>
#include <string>
#include <ddl/stream.h>
#include <ddl/utils.h>

#include "main_parser.h"

int main() {

  DDL::ParseError<DDL::Stream> err;
  DDL::ResultOf::parseMain result;
  bool success;

  DDL::ParserThread pt
    { "stdin"
    , [&err,&result,&success](DDL::Stream s) {
        success = DDL::parseOne(parseMain, err, &result, s);
      }
    };
  pt.resume();

  while (!pt.isDone()) {
    auto *s = new std::string();
    std::cin >> *s;
    auto n = s->length();
    if (n == 0) {
      delete s;
      pt.finish();
    } else {
      pt.append(n,s->c_str(),[s]() { delete s; });
    }
    pt.resume();
  }

  if (success) {
    std::cout << result;
    result.free();
  } else {
    std::cout << err;
  }

  return 0;
}
