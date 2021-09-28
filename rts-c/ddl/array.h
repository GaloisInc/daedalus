#ifndef DDL_ARRAY_H
#define DDL_ARRAY_H

#include <string.h>

#include <ddl/debug.h>
#include <ddl/list.h>
#include <ddl/number.h>
#include <ddl/size.h>

namespace DDL {

template <typename T>
class Array : IsBoxed {

  class Content {
    friend Array;
    RefCount  ref_count;
    Size      size;
    T         data[];


  public:

    // Allocate an array with unitialized data
    static
    Content *allocate(Size n) {
      size_t bytes = sizeof(Content) + sizeof(T[n.rep()]);
      char *raw = new char[bytes];    // XXX: alignment?
      Content *p   = (Content*) raw;
      p->ref_count = 1;
      p->size      = n;
      return p;
    }

  } *ptr;

  Array(Content *p) : ptr(p) {}

  void fill(Size) {}

  template <typename ... Elems>
  void fill(Size i, T x, Elems ... xs) {
    ptr->data[i.rep()] = x;
    fill(i.increment(),xs...);
  }

  static
  Size rangeSize(Size space, Size step) {
    Size ents  = Size{space.rep() / step.rep()};
    if (space.rep() % step.rep() > 0) ents.increment();
    return ents;
  }

public:

  static Array rangeUp(T start,T end, T step) {
    // step > 0 && step <= MAX(Size)
    // start <= end
    // (end - start) <= MAX(Size)

    Size ents = rangeSize((end - start).asSize(), step.asSize());
    Content *p = Content::allocate(ents);
    T val = start;
    for (Size i = 0; i < ents; i.increment()) {
      p->data[i.rep()] = val;
      val = val + step;
    }

    return Array(p);
  }

  static Array rangeDown(T start,T end, T step) {
    // step > 0 && step <= MAX(Size)
    // start >= end
    // (start - end) <= MAX(Size)

    Size ents = rangeSize((start - end).asSize(), step.asSize());
    Content *p = Content::allocate(ents);
    T val = start;
    for (Size i = 0; i < ents; i.increment()) {
      p->data[i.rep()] = val;
      val = val - step;
    }

    return Array(p);
  }



  using Builder = List<T>;


  Array() : ptr(NULL) {}

  // Array literal. Owns xs
  template <typename ...Elems>
  Array(Size n, Elems ... xs)
    : ptr(Content::allocate(n)) { fill(Size{0},xs...); }

  // Array from builder. Owns xs
  Array(Builder xs) {
    Size n = xs.size();
    ptr = Content::allocate(n);
    for (T *arr = ptr->data; n > 0; n.decrement()) {
      List<T> ys;
      xs.uncons(arr[n.decremented()],ys);
      xs = ys;
    }
  }

  // Concat. Borrows xs
  Array(Array<Array<T>> xs) {
    const Size outSize = xs.size();
    Size inSize  = 0;
    for (Size i = 0; i < outSize; i.increment()) {
      inSize.incrementBy(xs.borrowElement(i).size());
    }
    ptr = Content::allocate(inSize);

    T* data = ptr->data;

    for (Size i = 0; i < outSize; i.increment()) {
      Array a = xs.borrowElement(i);
      Size n = a.size();
      for (Size j = 0; j < n; j.increment()) {
        *data = a[j];
        ++data;
      }
    }
  }

  // Borrow arguments
  Array(T *data, Size n) : ptr(Content::allocate(n)) {
    memcpy(ptr->data, data, sizeof(T[n.rep()]));
  }

  // Borrows this
  Size size() { return ptr->size; }

  // Borrow this.
  // Returns a borrowed version of to element (if reference)
  T borrowElement(Size i) { return ptr->data[i.rep()]; }

  // Borrows this
  // Returns an owned copy of the element.
  T operator[] (Size i0) {
    size_t i = i0.rep();
    if constexpr (std::is_base_of<HasRefs,T>::value) {
      T& x = ptr->data[i];
      x.copy();
      return x;
    }
    return ptr->data[i];
  }


  T* borrowData() {
    return (T*)&ptr->data;
  }



// -- Boxed --------------------------------------------------------------------
  RefCount refCount() { return ptr->ref_count; }

  void copy() { ++(ptr->ref_count); }

  void free() {
    RefCount n = refCount();
    if (n == 1) {
      if constexpr (std::is_base_of<HasRefs,T>::value) {
        size_t todo = ptr->size.rep();
        T* arr = ptr->data;
        for(size_t i = 0; i < todo; ++i) arr[i].free();
      }
      debug("  Freeing array "); debugValNL((void*)ptr);
      delete[] (char*)ptr;
    } else {
      ptr->ref_count = n - 1;
    }
  }
// -- Boxed --------------------------------------------------------------------




  class Iterator : HasRefs {
    Size  index;
    Array xs;
    Iterator(Size i,Array ys) : index(i), xs(ys) {}
  public:
    Iterator() : index(0), xs() {}    // uninitialied

    // Owned xs
    Iterator(Array xs)  : index(0), xs(xs) {}

    bool   done()       { return index >= xs.size(); }
    DDL::UInt<64> key() { return DDL::UInt<64>(index.rep()); }
    // XXX: Update to size

    // Returns owned value
    T value()       { return xs[index]; }
    // Returns borrowed value
    T borrowValue() { return xs.borrowElement(index); }

    // Owned this. We don't increment `xs` because this copy of the iterator
    // is being freed.
    Iterator next() {
      return Iterator(index.increment(), xs);
    }

    void free() { xs.free(); }
    void copy() { xs.copy(); }

    // borrow
    friend
    std::ostream& operator<<(std::ostream& os, Array<T>::Iterator x) {
      os << "Iterator[" << x.index << "]";
      return os;
    }

  };



};

// borrow
template <typename T>
inline
std::ostream& operator<<(std::ostream& os, Array<T> x) {
  Size n = x.size();

  os << "[";
  char sep[] = ", ";
  sep[0] = 0;
  for (Size i = 0; i < n; i.increment()) {
    os << sep << x.borrowElement(i);
    sep[0] = ',';
  }
  os << "]";
  return os;
}


// borrow
template <typename T>
inline
std::ostream& toJS(std::ostream& os, Array<T> x) {
  Size n = x.size();

  os << "[";
  char sep[] = ", ";
  sep[0] = 0;
  for (Size i = 0; i < n; i.increment()) {
    toJS(os << sep, x.borrowElement(i));
    sep[0] = ',';
  }
  os << "]";
  return os;
}

// -ve: x < y; 0: x == y; +ve: x > y
// borrow arguments
template <typename T>
static inline
int compare (Array<T> x, Array<T> y) {
  Size size_x = x.size();
  Size size_y = y.size();
  Size checks = size_x < size_y ? size_x : size_y;
  for (Size i = 0; i < checks.rep(); i.increment()) {
    int result = compare(x.borrowElement(i),y.borrowElement(i));
    if (result != 0) return result;
  }
  return size_x > size_y ? (size_x.rep() - size_y.rep()) : -1;
}


// Borrow arguments
template <typename T> static inline
bool operator == (Array<T> xs, Array<T> ys) { return compare(xs,ys) == 0; }

// Borrow arguments
template <typename T> static inline
bool operator < (Array<T> xs, Array<T> ys) { return compare(xs,ys) < 0; }

// Borrow arguments
template <typename T> static inline
bool operator > (Array<T> xs, Array<T> ys) { return compare(xs,ys) > 0; }

// Borrow arguments
template <typename T> static inline
bool operator != (Array<T> xs, Array<T> ys) { return !(xs == ys); }

// Borrow arguments
template <typename T> static inline
bool operator <= (Array<T> xs, Array<T> ys) { return !(xs > ys); }

// Borrow arguments
template <typename T> static inline
bool operator >= (Array<T> xs, Array<T> ys) { return !(xs < ys); }





} // namespace DDL
#endif
