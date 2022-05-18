#ifndef DDL_OWNED_H
#define DDL_OWNED_H

namespace DDL {

template<class T>
class Owned {
    T obj;

    Owned() = delete;

public:
    // Owns x
    explicit Owned(T x) : obj(x) {};
    Owned(const Owned<T> &x) : obj(x.obj) { obj.copy(); }

    // Frees owned value
    ~Owned() { obj.free(); }

    Owned &operator=(const Owned &x) {
      obj.free();
      obj = x.obj;
      obj.copy();
      return *this;
    }

    // Accesses underlying owned value
    // This is a pointer to a borrowed value.
    T* operator->()             { return &obj; }
    T const* operator->() const { return &obj; }

    // Caller borrows result
    T borrow() const { return obj; }

    // Caller owns result
    T get() { obj.copy(); return obj; }
};

// Borrows its argument
template <class T>
Owned<T> borrowed(T x) {
  x.copy();
  return Owned{x};
}

}

#endif
