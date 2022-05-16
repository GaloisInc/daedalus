#ifndef DDL_PARSER_H
#define DDL_PARSER_H

#include <cstdint>
#include <iostream>
#include <vector>

#include <ddl/debug.h>
#include <ddl/input.h>
#include <ddl/stack.h>

namespace DDL {

typedef size_t ThreadId;

struct ParseError {
  // XXX: more info (stream information, some description)
  Size offset;
  std::vector<char const*> debugs;
};

class ParserState {

  Size                  fail_offset;    // largest, only makes sense if we fail
  std::vector<char const*> fail_debugs;

  ListStack             stack;
  std::vector<Thread>   suspended;
  std::vector<char const*> debugs;

public:
  ParserState() : fail_offset(0) {}

  ParseError getParseError() {
    return {fail_offset, fail_debugs};
  }

  // All alternatives failed.   Free the stack and return the
  // offset of the best error we computed.
  Size finalYield() {
    debugLine("final yield");
    debugVal(stack);
    stack.free();
    return fail_offset;
  }


  // ------------------------------------------------------------------------

  // For debug
  void say(const char *msg) { debugLine(msg); }

  void pushDebug(char const* msg) { debugs.push_back(msg); }
  void popDebug() { debugs.pop_back(); }

  // Set the "sibling failied" flag in the given thread.
  // Assumes: valid id (i.e., thread has not been resumed)
  void notify(ThreadId id) { suspended[id].notify(); }

  // Set the "furtherest fail" location.
  // XXX: This is not quite right because, in principle, the failures
  // may be in different inputs.  For the moment, we ignore this, which
  // could result in confusing error locations.
  void noteFail(Input input) {
    Size offset = input.getOffset();
    if (offset > fail_offset) {
      fail_offset = offset;
      fail_debugs = debugs;
    }
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
    suspended.push_back(Thread(c,stack));
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

    return stack.retAddr();
  }

};

}

#endif
