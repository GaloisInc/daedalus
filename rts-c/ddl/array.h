#ifndef DDL_ARRAY
#define DDL_ARRAY

#include <type_traits>
#include <ddl/list.h>

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
      Content *p   = (Content*) ::operator new(bytes);
      p->ref_count = 1;
      p->size      = n;
      std::cout << "Allocated " << p << std::endl;
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

public:
  Array() : ptr(NULL) {}

  // Array literal. Owns xs
  template <typename ...Elems>
  Array(size_t n, Elems ... xs) : ptr(Content::allocate(n)) { fill(0,xs...); }

  // Array from builder. Owns xs
  Array(List<T> xs) {
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



// -- Boxed --------------------------------------------------------------------
  size_t refCount() { return ptr->ref_count; }
  void copy()       { ++(ptr->ref_count); }
  void free() {
    size_t n = refCount();
    if (n == 1) {
      if constexpr (std::is_base_of<IsBoxed,T>::value) {
        size_t todo = ptr->size;
        T* arr = ptr->data;
        for(size_t i = 0; i < todo; ++i) arr[i].free();
      }
      std::cout << "Freeing " << ptr << std::endl;
      delete ptr;
    } else {
      ptr->ref_count = n - 1;
    }
  }
// -- Boxed --------------------------------------------------------------------


  // Borrows this
  size_t size() { return ptr->size; }


  // Borrow this.
  // Returns a borrowed version of to element (if reference)
  T borrowElement(size_t i) { return ptr->data[i]; }

  // Borrows this
  // Returns an owned copy of the element.
  T operator[] (size_t i) {
    if constexpr (std::is_base_of<IsBoxed,T>::value) {
      T& x = ptr->data[i];
      x.copy();
      return x;
    }
    return ptr->data[i];
  }

};

} // namespace DDL
#endif
