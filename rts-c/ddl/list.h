#ifndef DDL_LIST_H
#define DDL_LIST_H

#include <ddl/debug.h>
#include <ddl/boxed.h>

namespace DDL {

template <typename T>
class List : IsBoxed {

  class Node : HasRefs {
    Size size;    // length of the list
    T    head;
    List tail;
  public:
    friend List;
    Node(T&& h, List t) : size(t.size().incremented()), head(h), tail(t) {}

    void free() {
      if constexpr (std::is_base_of<HasRefs,T>::value) head.free();
      tail.free();
    }
  };

  Boxed<Node> ptr;

  List(Boxed<Node> p) : ptr(p) {}

public:

  // We represent the empty list as the "NIL" pointer
  // so we have to handle that specially in all function bellow.
  List() : ptr() {}

  // Cons cell; both arguments are owned
  List (T h, List t) : ptr(Node(std::move(h),t)) {}

  // Borrow "this"
  Size size() { return ptr.isNull()? Size{0} : ptr.getValue().size; }

  // Borrow "this"
  bool isNull() { return ptr.isNull(); }

  // Own "this"
  // Returns "Owned" `h` and `t`
  // `t` should not be this.
  void uncons(T &h, List& t) {
    Node &n = ptr.getValue();
    h = n.head;
    t = n.tail;
    if (refCount() == 1) {
      ptr.del();
    } else {
      if constexpr (std::is_base_of<HasRefs,T>::value) h.copy();
      t.copy();
      ptr.free();   // just decrement
    }

  }

  T&    borrowHead() { return ptr.getValue().head; }
  List& borrowTail() { return ptr.getValue().tail; }

  friend
  std::ostream& operator<<(std::ostream& os, List x) {
    os << "List " << x.ptr.rawPtr();
    return os;
  }




  // Reference counintg --------------------------------------------------

  // Empty list is always shared, so we return 2 if the pointer is NULL
  inline
  RefCount refCount() { return ptr.isNull()? 2 : ptr.refCount(); }
  void   free()     { if (!ptr.isNull()) ptr.free(); }
  void   copy()     { if (!ptr.isNull()) ptr.copy(); }
  void*  rawPtr()   { return ptr.rawPtr(); }
};




} // namespace DDL

#endif
