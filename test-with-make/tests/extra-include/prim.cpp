#include <main_parser.h>
#include <userState.hpp>
#include <iostream>

bool parser_P
  ( DDL::ParserStateUser<UserState> &p
  , DDL::UInt<8ul> *out
  , DDL::Input *outi
  , DDL::Input ini
  ) {
  UserState &s = p.getUserState();
  DDL::UInt<8> r = s.get();
  std::cout << r << std::endl;
  s.increment();
  *out  = r;
  *outi = ini;
  return true;
}
