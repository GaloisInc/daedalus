#pragma once

#include <ddl/parser.h>
#include <ddl/unit.h>
#include <ddl/input.h>
#include <ddl/number.h>
#include <ddl/exception.h>

// Some custom application state, accessible to the parser
class State {
  char special;
public:
  State(char x): special(x) {}
  void set(char y) { special = y; }
  char get() const { return special; }
};

// Implementation of the `SetSpecial` parser
inline DDL::ParserResult parser_SetSpecial(
    DDL::ParserStateUser<DDL::Input, State>& state,
    DDL::Unit* result, DDL::Input* newInput,
    DDL::Input currentInput, DDL::UInt<8> x) {
  state.getUserState().set((char)x.rep());      // modify the state
  *result = DDL::Unit();                        // parser result
  *newInput = currentInput;                     // parser input after parsing
  return DDL::ParserResult::Ok;                 // the parser succeeded
}

// Implementation of the `GetSpecial` parser
inline DDL::ParserResult parser_GetSpecial(
    DDL::ParserStateUser<DDL::Input, State>& state,
    DDL::UInt<8>* result, DDL::Input* newInput,
    DDL::Input currentInput) {
  *result = DDL::UInt<8>(state.getUserState().get());   // parser result
  *newInput = currentInput;                             // parser input after parsing
  return DDL::ParserResult::Ok;                         // the parser succeeded
}
