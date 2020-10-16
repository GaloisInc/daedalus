#ifndef DDL_BOXED
#define DDL_BOXED

#include <utility>
#include <iostream>

namespace DDL {

// Classes that own refernces should derive from this class so that
// we know to call `copy/free` on the member references.
// Note that this might happen without actually dallocating the object.
// An object that has been "freed" can't be used, but it space *may* be
// reusable to store other objects.
class HasRefs {};
// Classes should define:
// void free()
// void copy()


// Classes passed by reference.
// Just for documentation.
class IsBoxed : public HasRefs {};
// size_t refCount()



// A reference counted pointer.   Note that reference counting is done manually
// using the `copy` and `free` methods and NOT by
// copying constructors/assignment/destructors.
// This gives us more precise control over the life times of things.
// Note that there is no way to check if a reference is "dangling".
template <typename T>
class Boxed : IsBoxed {

  struct BoxedValue {
    size_t ref_count;
    T      value;
    BoxedValue()      : ref_count(1) {}
    BoxedValue(T &&x) : ref_count(1), value(x) {}
  } *ptr;

public:
  Boxed()      : ptr(NULL)                          {}
  Boxed(T&& x) : ptr (new BoxedValue(std::move(x))) {
    std::cout << "Allocated " << ptr << std::endl;
  }

  bool isNull() { return ptr == NULL; }

  size_t refCount() { return ptr->ref_count; }

  // Allocate without initializing the data, but ref count is 1
  void allocate() {
    ptr = new BoxedValue();
    std::cout << "Allocated " << ptr << std::endl;
  }

  // Release the memory for an object that has already been unitialized.
  void del() {
    std::cout << "Freeing " << ptr << std::endl;
    delete ptr;
   }

  // Relese this reference to the box.
  void free() {
    size_t n = ptr->ref_count;
    if (n == 1) {
      if constexpr (std::is_base_of<HasRefs,T>::value) ptr->value.free();
      del();
    }
    else ptr->ref_count = n - 1;
  }

  // Make a new "owned" copy of the referece (i.e., increase ref count).
  void copy() { ++(ptr->ref_count); }

  // Get access to the contents of the box.
  // The resulting reference shouldn't be used after the box is gone.
  T& getValue() { return ptr->value; }
};

}


#endif

