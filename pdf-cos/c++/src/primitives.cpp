#include <iostream>
#include <main_parser.h>

bool parse_ResolveRef
  ( DDL::ParserState &pstate
  , DDL::Maybe<User::TopDecl> *result
  , DDL::Input *out_input
  , DDL::Input input

  , User::Ref ref
  ) {

  *out_input = input;
  std::cout << "XXX: parse_ResolveRef\n";
  return false;
}


bool parse_Decrypt
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input

  , DDL::Input body
  ) {

  *out_input = input;
  std::cout << "XXX: parse_Decrypt\n";
  return false;
}


bool parse_FlateDecode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input

  , DDL::Integer predictor
  , DDL::Integer colors
  , DDL::Integer bpc
  , DDL::Integer columns
  , DDL::Input   body
  ) {

  *out_input = input;
  std::cout << "XXX: parse_FlateDecode\n";
  return false;
}


bool parse_LZWDecode
  ( DDL::ParserState &pstate
  , DDL::Input* result
  , DDL::Input* out_input
  , DDL::Input input

  , DDL::Integer predictor
  , DDL::Integer colors
  , DDL::Integer bpc
  , DDL::Integer columns
  , DDL::Integer earlychange
  , DDL::Input body
  ) {

  *out_input = input;
  std::cout << "XXX: parse_LZWDecode\n";
  return false;
}


bool parse_ASCIIHexDecode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  *out_input = input;
  std::cout << "XXX: parse_ASCIIHexDecode\n";
  return false;
}

bool parse_ASCII85Decode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  *out_input = input;
  std::cout << "XXX: parse_ASCII85Decode\n";
  return false;
}


