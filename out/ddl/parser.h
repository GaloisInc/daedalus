#ifndef DDL_PARSER_H
#define DDL_PARSER_H

#include <cstdint>
#include <iostream>
#include <vector>
#include <optional>

#include <ddl/debug.h>
#include <ddl/input.h>
#include <ddl/stack.h>
#include <ddl/parse_error.h>

namespace DDL {

typedef size_t ThreadId;

class ParserState {

  ParseError error;

  ListStack           stack;
  std::vector<Thread> suspended;
  ParserContextStack  debugs;

public:
  ParserState() {}

  ParseError getParseError() { return error; }

  // All alternatives failed.
  // Free the stack and return the best error we know about.
  ParseError finalYield() {
    debugLine("final yield");
    debugVal(stack);
    stack.free();
    return getParseError();
  }


  // ------------------------------------------------------------------------

  // For debug
  void say(const char *msg) { debugLine(msg); }

  void pushDebug(char const* msg, bool tail = false) {
    if (tail) debugs.tailCallFun(msg); else debugs.callFun(msg);
  }
  void popDebug() { debugs.popFun(); }

  // Set the "sibling failied" flag in the given thread.
  // Assumes: valid id (i.e., thread has not been resumed)
  void notify(ThreadId id) { suspended[id].notify(); }

  // Borrows input and msg
  void noteFail(bool is_sys, char const *loc, Input input, Array<UInt<8>> msg) {
    error.improve(is_sys,loc,input,msg,debugs);
  }

  // Function calls
  void push(Closure *c) { stack = ListStack{ClosureRef{c},stack}; }
  Closure* pop()        { return stack.pop(stack); }

  // Returns the address of the code for the continuation, the closure is on
  // top of the stack. If there were alternative continuations
  // (for the yes/no cases) the other alternative is removed from the stack.
  void* returnPure()  { return stack.retAddr(); }
  void* returnYes()   { stack = stack.squish(); return stack.retAddr(); }
  void* returnNo()    { stack.pop(stack)->free(); return stack.retAddr(); }


  // -- Threads ---------------------------------------------------------------

  // Add to waiting threads
  ThreadId spawn(ThreadClosure *c) {
    ThreadId id = suspended.size();
    debug("spawning thread "); debugValNL(id);
    stack.copy();
    debugVal(stack);
    suspended.push_back(Thread(c,stack,debugs));
    return id;
  }

  // True if there are there are threads to resume
  bool hasSuspended() { return !suspended.empty(); }


  // Assumes that there is a suspended thread.
  // Returns the address of the code for the continuation.
  // The top suspended thread is removed:
  //    * its stack replaces the current stack
  //    * its closure is pushed on the *new* current stack.
  void *yield() {
    debugLine("yielding");
    Thread& t = suspended.back();

    debugLine("freeing stack");
    debugVal(stack);

    stack.free();
    ThreadClosure *c = t.closure;
    stack = ListStack(c,t.stack);
    suspended.pop_back();

    debugLine("new stack");
    debugVal(stack);

    debugs = t.debug;

    return stack.retAddr();
  }

};

}

#endif
