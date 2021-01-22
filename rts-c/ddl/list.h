#ifndef DDL_LIST_H
#define DDL_LIST_H

#include <type_traits>
#include <ddl/boxed.h>

namespace DDL {

template <typename T>
class List : IsBoxed {

  class Node : HasRefs {
    size_t size;    // lenght of the list
    T    head;
    List tail;
  public:
    friend List;
    Node(T   h, List t) : size(1+t.size()), head(h), tail(t) {}

    void free() {
      std::cout << "  1.free node head\n";
      if constexpr (std::is_base_of<HasRefs,T>::value) head.free();
      std::cout << "  2.free node tail\n";
      tail.free();
      std::cout << "  3.free node done\n";
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
  size_t size() { return ptr.isNull()? 0 : ptr.getValue().size; }

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
  size_t refCount() { return ptr.isNull()? 2 : ptr.refCount(); }
  void   free()     { if (!ptr.isNull()) ptr.free(); }
  void   copy()     { if (!ptr.isNull()) ptr.copy(); }
  void*  rawPtr()   { return ptr.rawPtr(); }
};




} // namespace DDL

#endif
