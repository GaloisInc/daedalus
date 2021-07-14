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

static inline
std::ostream& operator<<(std::ostream& os, Closure& x) {
  os << "[clo|" << x.ref_count << "|" << (void*)&x << "|" << x.code << "]";
  return os;
}

class ClosureRef : HasRefs {
  Closure *ptr;
public:
  ClosureRef() : ptr(nullptr) {}
  ClosureRef(Closure *p) : ptr(p) {}    // never null

  void copy()         { ptr->copy(); }
  void free()         { ptr->free(false); }
  Closure *getValue() { return ptr; }
};

static inline
std::ostream& operator<<(std::ostream& os, ClosureRef x) {
  return os << *x.getValue();
}



class ListStack : HasRefs {
  List<ClosureRef> data;    // XXX: can combine a node a closure
  ListStack(List<ClosureRef> xs) : data(xs) {}

public:
  ListStack() : data() {}

  // owns both arguments
  ListStack(ClosureRef c, ListStack s)
    : data(List<ClosureRef>{c,s.data}) {}

  // own this
  // \(x:xs) -> xs
  Closure *pop(ListStack& out) {
    ClosureRef x;
    List<ClosureRef> xs;
    data.uncons(x,xs);
    out = ListStack{xs};
    return x.getValue();
  }

  // owns this
  // \(x : xs@(y : ys) -> x : ys
  ListStack squish() {
    ClosureRef x, y;
    List<ClosureRef> xs, ys;
    data.uncons(x,xs);
    xs.uncons(y,ys);
    y.free();
    return ListStack{x,ys};
  }

  void* retAddr() { return data.borrowHead().getValue()->code; }

  void free() { data.free(); }
  void copy() { data.copy(); }

  friend
  std::ostream& operator<<(std::ostream& os, const ListStack & x) {
    os << "[stack]\n";
    List<ClosureRef> p = x.data;
    while (! p.isNull()) {
      os << "  [" << (void*)p.rawPtr() << "](" << p.refCount() << ")" << p.borrowHead() << " -> ";
      p = p.borrowTail();
      os << "next " << (void*) p.rawPtr() << std::endl;
    }
    os << "[/stack]\n";
    return os;
  }

};


// We only even have a single pointer to a thread closure, from
// withing the parser's stack object.
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
