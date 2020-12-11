#include<ddl/boxed.h>
#include<ddl/stack.h>
#include<ddl/parser.h>

// This shows how generate code for functions calls



void example() {

  DDL::Parser<int> p{DDL::Input()};

  int f_arg;         // Pass arguments to `f` here
  int other_arg = 5;     // This shoudl be prserved across a call to `f`



  // A stack frame preserving values across a call
  struct Return_F : public DDL::Closure {
    int _0;

    Return_F(void *code, int x0) : DDL::Closure(code), _0(x0) {}

    void freeMembers() {}
  };

  struct SampleThread : public DDL::ThreadClosure {
    int _0;
    SampleThread(void *code, int x0) : DDL::ThreadClosure(code), _0(x0) {}
    void freeMembers() {}
  };


  goto start;

T:
  std::cout << "now in T\n";
  { SampleThread *frame = (SampleThread*) p.pop();
    other_arg = frame->_0;
    std::cout << "notified = " << frame->notified << std::endl;;
    frame->free(true);
  }
  goto *p.returnPure();


F:
  std::cout << "Function F(" << f_arg << ")\n";
  other_arg = 77;
  p.spawn(new SampleThread(&&T,other_arg));
  std::cout << "F: other_arg = " << other_arg << std::endl;
  // std::cout << "F: stack = " << stack << std::endl;
  goto *p.returnPure();


start:
  std::cout << "start: other_arg = " << other_arg << std::endl;

  // A function call
  p.push(new Return_F(&&F_ret,other_arg));
  f_arg = 5;
  goto F;

F_ret: // This label needs a new name as it is not scoped.
  std::cout << "Returned from F\n";
  { Return_F *frame = (Return_F*) p.pop();
    other_arg = frame->_0;   // restore arguments
    frame->free(true);
  }

  if (p.hasSuspended()) {
    std::cout << "Now we'll try to resume\n";
    goto *p.yield();
  } else {
    std::cout << "The end.\n";
    goto end;
  }


end:
  std::cout << "end: other_arg = " << other_arg << std::endl;
  // std::cout << "end: stack = " << stack << std::endl;
  return;
}


