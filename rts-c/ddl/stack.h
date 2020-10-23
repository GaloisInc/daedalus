#ifndef DDL_STACK
#define DDL_STACK

#include <ddl/boxed.h>

namespace DDL {

// Common parts of a stack frame.
struct StackFrame {
  void *code;
  void *next;
};

using Stack = BoxedValue<StackFrame>*;

static inline
void* ReturnPure(void* stack) {
  Stack f = (Stack) stack;
  return f->value.code;
}



}
#endif
