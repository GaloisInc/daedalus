#ifndef DDL_LIST
#define DDL_LIST

#include <type_traits>
#include <ddl/boxed.h>

namespace DDL {

template <typename T>
class List : IsBoxed {

  class Node {
    size_t size;    // lenght of the list
    T    head;
    List tail;
  public:
    friend List;
    Node(T &&h, List t) : size(1+t.size()), head(h), tail(t) {}
    Node(T   h, List t) : size(1+t.size()), head(h), tail(t) {}
  };

  Boxed<Node> ptr;

  List(Boxed<Node> p) : ptr(p) {}

public:

  // We represent the empty list as the "NIL" pointer
  // so we have to handle that specially in all function bellow.
  List() : ptr() {}

  // Cons cell; both arguments are owned
  List (T &&h, List t) : ptr(Node(std::move(h),t)) {}
  List (T   h, List t) : ptr(Node(h,t)) {}

  // Borrow "this"
  size_t size() { return ptr.isNull()? 0 : ptr.getValue().size; }

  // Own "this"
  // Returns "Owned" `h` and `t`
  // `t` should not be this.
  void uncons(T &h, List& t) {
    Node &n = ptr.getValue();
    h = n.head;
    t = n.tail;


    // We can't just do `free` because we don't want to
    // decrement `h` and `t`'s reference counts as they are preserved.
    if (refCount() > 1) {
      if constexpr (std::is_base_of<HasRefs,T>::value) h.copy();
      t.copy();
      ptr.free();
    } else {
      ptr.del();
    }
  }

  // Reference counintg --------------------------------------------------

  // Empty list is always shared, so we return 2 if the pointer is NULL
  inline
  size_t refCount() { return ptr.isNull()? 2 : ptr.refCount(); }

  void free() {
    if (ptr.isNull()) return;

    if (refCount() == 1) {
      Node &cell = ptr.getValue();
      if constexpr (std::is_base_of<HasRefs,T>::value) cell.head.free();
      cell.tail.free(); // note that we use stack here.
    }
    ptr.free();
  }

  void copy() { if (!ptr.isNull()) ptr.copy(); }

};

} // namespace DDL

#endif
