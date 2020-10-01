#ifndef DDL_BOXED
#define DDL_BOXED

namespace DDL {


// A reference counted pointer.   Note that reference counting is done manually
// using the `copy` and `free` methods and NOT by
// copying constructors/assignment/destructors.
// This gives us more precise control over the life times of things.
// Note that there is no way to check if a reference is "dangling".
template <typename T>
class Boxed {

  struct BoxedValue {
    size_t ref_count;
    T      value;
    BoxedValue(T &&x) : ref_count(1), value(x) {}
  } *ptr;

public:
  Boxed()      : ptr(NULL)                          {}
  Boxed(T&& x) : ptr (new BoxedValue(std::move(x))) {}

  size_t refCount() { return ptr->ref_count; }

  // Relese this reference to the box.
  void free() {
    size_t n = ptr->ref_count;
    --n;
    if (n == 0) delete ptr;
    else ptr->ref_count = n;
  }

  // Make a new "owned" copy of the referece (i.e., increase ref count).
  Boxed copy() {
    ++(ptr->ref_count);
    return *this;
  }

  // Get access to the contents of the box.
  // The resulting reference shouldn't be used after the box is gone.
  T& getValue() { return ptr->value; }
};



}


#endif

