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

    static
    Content *fromList(List<T> xs) {
      size_t n     = xs.size();
      size_t bytes = sizeof(Content) + sizeof(T[n]);
      Content *p   = (Content*) ::operator new(bytes);
      std::cout << "Allocated " << p << std::endl;

      p->size = n;
      p->ref_count = 1;
      for (T *arr = p->data; n > 0; --n) {
        List<T> ys;
        xs.uncons(arr[n-1],ys);
        xs = ys;
      }

      return p;
    }
  } *ptr;

  Array(Content *p) : ptr(p) {}

public:
  Array()           : ptr(NULL) {}                    // Uninitialized

  // Owns xs
  Array(List<T> xs) : ptr(Content::fromList(xs)) {}   // From a list

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

  // Borrows this
  // Returns a borrowed copy of the element.
  T operator[] (size_t i) { return ptr->data[i]; }


};

} // namespace DDL
#endif
