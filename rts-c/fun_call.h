#include<ddl/boxed.h>
#include<ddl/stack.h>

// This shows how generate code for functions calls



void example() {



  void *stack = NULL; // Used for function calls

  int f_arg;         // Pass arguments to `f` here
  int other_arg = 5;     // This shoudl be prserved across a call to `f`

  // Things we need to save
  struct Return_F : public DDL::StackFrame {
    int _0;

    static void* allocate(void *code, void *stack, int x0) {
      DDL::BoxedValue<Return_F> *frame = new DDL::BoxedValue<Return_F>();
      frame->value.code = code;
      frame->value.next = stack;
      frame->value._0   = x0;
      return frame;
    }
    void copy() {}    // would not be empty if we saved some references
    void free() {}
  };



  goto start;



F:
  std::cout << "Function F(" << f_arg << ")\n";
  other_arg = 77;
  std::cout << "F: other_arg = " << other_arg << std::endl;
  std::cout << "F: stack = " << stack << std::endl;
  goto * DDL::ReturnPure(stack);



start:
  std::cout << "start: other_arg = " << other_arg << std::endl;
  std::cout << "start: stack = " << stack << std::endl;

  // A function call
  stack = Return_F::allocate(&&F_ret,stack,other_arg);
  f_arg = 5;
  goto F;

F_ret: // This label needs a new name as it is not scoped.
  std::cout << "Returned from F\n";
  { DDL::BoxedValue<Return_F> *frame = (DDL::BoxedValue<Return_F>*)stack;
    other_arg = frame->value._0;   // restore arguments
    stack     = frame->value.next; // pop stack
    free_boxed(frame);
  }
  goto end;    // or whatever would happen after the call


end:
  std::cout << "end: other_arg = " << other_arg << std::endl;
  std::cout << "end: stack = " << stack << std::endl;
  return;
}


