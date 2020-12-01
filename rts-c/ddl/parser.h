#ifndef DDL_PARSER_H
#define DDL_PARSER_H

#include <cstdint>
#include <iostream>
#include <vector>

#include <ddl/input.h>
#include <ddl/stack.h>

namespace DDL {

typedef size_t ThreadId;

// `T` is the type of the result for the entry-point parser.
// The methods in this class correspond to the VM instructions, c.f. Daedalus.VM
template <class T>
class Parser {

  Input                 input;
  std::vector<T>        results;
  size_t                fail_offset;    // largest, only if `results` empty
  Stack                 stack;
  std::vector<Thread>   suspended;


public:

  // Argument is owned
  Parser(Input i) : input(i) , fail_offset(0) {}

  std::vector<T>& getResults()    { return results; }
  size_t          getFailOffset() { return fail_offset; }


  // ------------------------------------------------------------------------

  // Argument is owned
  void setInput(Input i) { input.free(); input = i;
    std::cout << "offset = " << input.getOffset() << std::endl;
    }

  // Returns a copy of the current input (result is owned)
  Input getInput() { input.copy(); return input; }

  // For debug
  void say(const char *msg) { std::cout << msg << std::endl; }

  // Called when we find a successful parse
  void output(T v) { results.push_back(v); }


  // Set the "sibling failied" flag in the given thread.
  // Assumes: valid id (i.e., thread has not been resumed)
  void notify(ThreadId id) { suspended[id].notify(); }

  // Set the "furtherest fail" location.
  // XXX: This is not quite right because, in principle, the failures
  // may be in different inputs.  For the moment, we ignore this, which
  // could result in confusing error locations.
  void noteFail() {
    size_t offset = input.getOffset();
    if (offset > fail_offset) fail_offset = offset;
  }

  // Function calls
  void push(Closure *c) { stack.push(c); }
  Closure* pop()        { return stack.pop(); }

  // Returns the address of the code for the continuation, the closure is on
  // top of the stack. If there were alternative continuations
  // (for the yes/no cases) the other alternative is removed from the stack.
  void* returnPure()    { return stack.retAddr(); }
  void* returnYes()     { stack.squish(); return stack.retAddr(); }
  void* returnNo()      { stack.pop()->free(false); return stack.retAddr(); }


  // -- Threads ---------------------------------------------------------------

  // Add to waiting threads
  ThreadId spawn(ThreadClosure *c) {
    ThreadId id = suspended.size();
    std::cout << "spawning thread " << id << std::endl;
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
    std::cout << "yielding\n";
    Thread& t = suspended.back();
    stack.overwriteBy(t.stack);
    ThreadClosure *c = t.closure;
    stack.push(c);
    suspended.pop_back();
    return stack.retAddr();
  }

};

}

#endif
