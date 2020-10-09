#ifndef DDL_BOXED
#define DDL_BOXED

namespace DDL {


// Boxed types should derive from this class, so that we can tell when
// to deallocated.
class IsBoxed {};
// Classes should define:
// void free()
// void copy()
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
    BoxedValue(T &&x) : ref_count(1), value(x) {}
  } *ptr;

public:
  Boxed()      : ptr(NULL)                          {}
  Boxed(T&& x) : ptr (new BoxedValue(std::move(x))) {
    std::cout << "Allocated " << ptr << std::endl;
  }

  bool isNull() { return ptr == NULL; }

  size_t refCount() { return ptr->ref_count; }

  // Release the memory without doing anything else
  void del() {
    std::cout << "Freeing " << ptr << std::endl;
    delete ptr;
   }

  // Relese this reference to the box.
  void free() {
    size_t n = ptr->ref_count;
    if (n == 1) {
      if constexpr (std::is_base_of<IsBoxed,T>::value) ptr->value.free();
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

