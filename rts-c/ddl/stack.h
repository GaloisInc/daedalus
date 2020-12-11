#ifndef DDL_STACK_H
#define DDL_STACK_H

#include <utility>
#include <vector>
#include <ddl/boxed.h>

namespace DDL {

// Stack frames extend this class with the parameters that need to be saved.
struct Closure {
  size_t ref_count;
  void   *code;

  Closure(void *c) : ref_count(1), code(c) { }
  virtual ~Closure() {}
  virtual void freeMembers() = 0;

  void free(bool shallow) {
    if (ref_count == 1) {
      if (!shallow) freeMembers();
      delete this;
    } else {
      --ref_count;
    }
  }

  void copy() {
    ++ref_count;
  }

};

class ListStack : HasRefs {
  List<Closure*> data;    // XXX: can combine a node a closure
  ListStack(List<Closure*> p) : data(p) {}

public:
  ListStack() : data() {}

  // owns both arguments
  ListStack(Closure *c, ListStack s) : data(List<Closure*>{c,s.data}) {}

  // own this
  // \(x:xs) -> xs
  Closure* pop(ListStack& out) {
    Closure *x;
    List<Closure*> xs;
    data.uncons(x,xs);
    out = ListStack{xs};
    return x;
  }

  // owns this
  // \(x : xs@(y : ys) -> x : ys
  ListStack squish() {
    Closure *x, *y;
    List<Closure*> xs, ys;
    data.uncons(x,xs);
    xs.uncons(y,ys);
    y->free(false);
    return ListStack(List{x,ys});
  }

  void* retAddr() { return data.borrowHead()->code; }

  void free() { data.free(); }
  void copy() { data.copy(); }

};

struct ThreadClosure : public Closure {
  bool notified;
  ThreadClosure(void *c) : Closure(c), notified(false) {}

  void notify() { notified = true; }
};

struct Thread {
  ThreadClosure *closure;
  ListStack stack;

public:
  Thread(ThreadClosure *c, const ListStack s) : closure(c), stack(s) {}
  void notify() { closure->notify(); }

};


}
#endif
