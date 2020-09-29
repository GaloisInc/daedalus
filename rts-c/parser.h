#ifndef DDL_PARSER_H
#define DDL_PARSER_H

#include <cstdint>
#include <iostream>
#include <vector>

#include "input.h"
// #include "closure.h"
// #include "ddl_thread.h"

namespace DDL {

typedef size_t ThreadId;

// `T` is the type of the result for the entry-point parser.
// The methods in this class correspond to the VM instructions, c.f. Daedalus.VM
template <class T>
class Parser {

  Input           input;
  std::vector<T>  results;
  size_t          fail_offset;

public:

  Parser(Input i)
    : input(i)
    , fail_offset(0)
  {}

  void setInput(Input i) { input = i; }

  // Returns a copy of the current input
  Input getInput() { return input; }

  // For debug
  void say(const char *msg) { std::cout << msg << std::endl; }

  // Called when we find a successful parse
  void output(T v) { results.push_back(v); }

  // Add to wainting threads
  // We become the owner of the closure.
  // ThreadId spawn(std::unique_ptr<Closure1<bool>> clo) {
  //   ThreadId id = suspended.size();
  //   suspended.push_back(Thread(clo));
  //   return id;
  // }

  // XXX:
  bool hasSuspended() { return false; }

  // Assumes that there is a suspended thread.
  void yield() {
    // auto t = suspended.back();
    // auto b = t.getNotified();
    // auto c = t.getClosure();
    // suspended.pop_back();
    // (*c)(b);
  }

  // Set the "sibling failied" flag in the given thread.
  // Assumes: valid id (i.e., thread has not been resumed)
  // void notify(ThreadId id) { suspended[id].notify(); }

  // Set the "furtherest fail" location.
  // XXX: This is not quite right because, in principle, the failurs
  // may be in different inputs.  For the moment, we ignore this, which
  // could result in confusing error locations.
  void noteFail() {
    size_t offset = input.getOffset();
    if (offset > fail_offset) fail_offset = offset;
  }

};

}

#endif
