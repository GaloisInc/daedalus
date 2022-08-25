#include <main_parser.h>
#include <iostream>

bool parser_P
  ( DDL::ParserStateUser<int> &p
  , DDL::UInt<8ul> *out
  , DDL::Input *outi
  , DDL::Input ini
  ) {

  int &s = p.getUserState();
  std::cout << "parser P " << s << "\n";
  ++s;
  *out = DDL::UInt<8>(72);
  *outi = ini;
  return true;
}


