#ifndef DDL_STACK
#define DDL_STACK

#include <utility>
#include <vector>
#include <ddl/boxed.h>

namespace DDL {

// Stack frames extend this class with the parameters that need to be saved.
struct Closure {
  size_t ref_count;
  void   *code;

  Closure(void *c) : ref_count(1), code(c) {}
  virtual ~Closure() {}
  virtual void freeMembers() = 0;

  void free() {
    if (ref_count == 1) {
      freeMembers();
      std::cout << "Deallocating " << this << std::endl;
      delete this;
    } else {
      --ref_count;
    }
  }

  void copy() {
    ++ref_count;
  }

};

class Stack {
  std::vector<Closure*> stack;

public:
  // Empty stack
  Stack() {}

  // Make a copy of a stack for storing in a thread.
  Stack(const Stack& other) : stack(other.stack) {
    for (size_t i = 0; i < stack.size(); ++i) {
      stack[i]->copy();
    }
  }

  // Move the other stack into this one.
  void overwriteBy(Stack& other) {
    for (size_t i = 0; i < stack.size(); ++i) {
      stack[i]->free();
    }
    stack.resize(other.stack.size());
    for (size_t i = 0; i < other.stack.size(); ++i) {
      stack[i] = other.stack[i];
    }
  }

  // Push a new stack on the stack
  void push(Closure *e) { stack.push_back(e); }

  // Remove the top element from the stack and return it.
  // Stack should not be empty.
  Closure *pop() {
    Closure *p = stack.back();
    stack.pop_back();
    return p;
  }

  // Remove the element *under* the top.
  // The stack should have at least 2 elements.
  // This is used when we have a choice of 2 continuations
  // (the top 2 elements of the stack).
  //  * To choose the top one we first `sqush` the one under then `pop`
  //  * To use the one under we just pop twice
  void squish() {
    Closure *p = pop();
    Closure * &q = stack.back();
    q->free();
    q = p;
  }

  // Get the return address for the frame on top of the stack.
  // The frame is not poped.
  // There must be at least one 1 frame on the stack.
  void* retAddr() { return stack.back()->code; }

};


struct ThreadClosure : public Closure {
  bool notified;
  ThreadClosure(void *c) : Closure(c), notified(false) {}

  void notify() { notified = true; }
};

struct Thread {
  ThreadClosure *closure;
  Stack    stack;

public:
  Thread(ThreadClosure *c, const Stack& s) : closure(c), stack(s) {}
  void notify() { closure->notify(); }

};


}
#endif
