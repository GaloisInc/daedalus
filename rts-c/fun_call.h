#include<ddl/boxed.h>

// This shows how generate code for functions calls

// Common parts of a stack frame.
struct StackFrame {
  void *code;
  void *next;
};


void example() {




  void *stack = NULL; // Used for function calls

  int f_arg;         // Pass arguments to `f` here
  int other_arg = 5;     // This shoudl be prserved across a call to `f`

  goto start;



F:
  std::cout << "Function F(" << f_arg << ")\n";
  other_arg = 77;
  std::cout << "F: other_arg = " << other_arg << std::endl;
  std::cout << "F: stack = " << stack << std::endl;
  goto * ((DDL::BoxedValue<StackFrame>*)stack)->value.code;   // return



start:
  std::cout << "start: other_arg = " << other_arg << std::endl;
  std::cout << "start: stack = " << stack << std::endl;

  // A function call
  {
    // Things we need to save
    struct Call : public StackFrame, public DDL::HasRefs {
      int other_arg;

      void copy() {}    // would not be empty if we saved some references
      void free() {}
    };

    // Save stuff
    { DDL::BoxedValue<Call> *frame = new DDL::BoxedValue<Call>();
      std::cout << "allocated frame " << frame << std::endl;
      frame->value.code = &&F_ret;
      frame->value.next = stack;
      frame->value.other_arg = other_arg;
      stack = frame;
    }

    f_arg = 5;
    goto F;

    F_ret: // This label needs a new name as it is not scoped.
      std::cout << "Returned from F\n";
      DDL::BoxedValue<Call> *frame = (DDL::BoxedValue<Call>*)stack;
      other_arg = frame->value.other_arg;   // restore arguments
      stack = frame->value.next;            // pop stack
      free_boxed(frame);
    goto end;    // or whatever would happen after the call
  }


end:
  std::cout << "end: other_arg = " << other_arg << std::endl;
  std::cout << "end: stack = " << stack << std::endl;
  return;
}


