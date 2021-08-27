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
    size_t ref_count;
    size_t size;
    T      data[];


  public:

    // Allocate an array with unitialized data
    static
    Content *allocate(size_t n) {
      size_t bytes = sizeof(Content) + sizeof(T[n]);
      char *raw = new char[bytes];    // XXX: alignment?
      Content *p   = (Content*) raw;
      p->ref_count = 1;
      p->size      = n;
      return p;
    }

  } *ptr;

  Array(Content *p) : ptr(p) {}

  void fill(size_t) {}

  template <typename ... Elems>
  void fill(size_t i, T x, Elems ... xs) {
    ptr->data[i] = x;
    fill(i+1,xs...);
  }

  static
  size_t rangeSize(size_t space, size_t step) {
    size_t ents  = space / step;
    size_t extra = space % step;
    if (extra > 0) ++ents;
    return ents;
  }

public:

  static Array rangeUp(T start,T end, T step) {
    // step > 0 && step <= MAX(size_t)
    // start <= end
    // (end - start) <= MAX(size_t)

    size_t ents = rangeSize((end - start).asSize().rep(), step.asSize().rep());
    Content *p = Content::allocate(ents);
    T val = start;
    for (size_t i = 0; i < ents; ++i) {
      p->data[i] = val;
      val = val + step;
    }

    return Array(p);
  }

  static Array rangeDown(T start,T end, T step) {
    // step > 0 && step <= MAX(size_t)
    // start >= end
    // (start - end) <= MAX(size_t)

    size_t ents = rangeSize((start - end).asSize().rep(), step.asSize().rep());
    Content *p = Content::allocate(ents);
    T val = start;
    for (size_t i = 0; i < ents; ++i) {
      p->data[i] = val;
      val = val - step;
    }

    return Array(p);
  }



  using Builder = List<T>;


  Array() : ptr(NULL) {}

  // Array literal. Owns xs
  template <typename ...Elems>
  Array(size_t n, Elems ... xs) : ptr(Content::allocate(n)) { fill(0,xs...); }

  // Array from builder. Owns xs
  Array(Builder xs) {
    size_t n = xs.size();
    ptr = Content::allocate(n);
    for (T *arr = ptr->data; n > 0; --n) {
      List<T> ys;
      xs.uncons(arr[n-1],ys);
      xs = ys;
    }
  }

  // Concat. Borrows xs
  Array(Array<Array<T>> xs) {
    size_t outSize = xs.size();
    size_t inSize  = 0;
    for (size_t i = 0; i < outSize; ++i) {
      inSize += xs.borrowElement(i).size();
    }
    ptr = Content::allocate(inSize);

    T* data = ptr->data;

    for (size_t i = 0; i < outSize; ++i) {
      Array a = xs.borrowElement(i);
      size_t n = a.size();
      for (size_t j = 0; j < n; ++j) {
        *data = a[j];
        ++data;
      }
    }
  }

  // Borrow arguments
  Array(T *data, size_t n) : ptr(Content::allocate(n)) {
    memcpy(ptr->data, data, sizeof(T[n]));
  }

  // Borrows this
  size_t size() { return ptr->size; }

  // XXX: Remove in favor of size
  // Borrow this.
  // Returns a borrowed version of to element (if reference)
  T borrowElement(UInt<64> i) { return ptr->data[i.rep()]; }


  // XXX: Removein favor of size
  // Borrows this
  // Returns an owned copy of the element.
  T operator[] (UInt<64> i0) {
    auto i = i0.rep();
    if constexpr (std::is_base_of<HasRefs,T>::value) {
      T& x = ptr->data[i];
      x.copy();
      return x;
    }
    return ptr->data[i];
  }


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
  size_t refCount() { return ptr->ref_count; }

  void copy() { ++(ptr->ref_count); }

  void free() {
    size_t n = refCount();
    if (n == 1) {
      if constexpr (std::is_base_of<HasRefs,T>::value) {
        size_t todo = ptr->size;
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
    size_t index;
    Array xs;
    Iterator(size_t i,Array ys) : index(i), xs(ys) {}
  public:
    Iterator() : index(0), xs() {}    // uninitialied

    // Owned xs
    Iterator(Array xs)  : index(0), xs(xs) {}

    bool   done()       { return index >= xs.size(); }
    DDL::UInt<64> key() { return DDL::UInt<64>(index); }
    // XXX: Update to size

    // Returns owned value
    T value()       { return xs[index]; }
    // Returns borrowed value
    T borrowValue() { return xs.borrowElement(index); }

    // Owned this. We don't increment `xs` because this copy of the iterator
    // is being freed.
    Iterator next() {
      return Iterator(index + 1, xs);
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
  size_t n = x.size();

  os << "[";
  char sep[] = ", ";
  sep[0] = 0;
  for (size_t i = 0; i < n; ++i) {
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
  size_t n = x.size();

  os << "[";
  char sep[] = ", ";
  sep[0] = 0;
  for (size_t i = 0; i < n; ++i) {
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
  size_t size_x = x.size();
  size_t size_y = y.size();
  size_t checks = size_x < size_y ? size_x : size_y;
  for (size_t i = 0; i < checks; ++i) {
    int result = compare(x.borrowElement(i),y.borrowElement(i));
    if (result != 0) return result;
  }
  return size_y - size_x;
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
