#ifndef DDL_BOXED_H
#define DDL_BOXED_H

#include <ddl/size.h>
#include <ddl/debug.h>

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
// RefCount refCount()


template <typename T>
struct BoxedValue {
  RefCount  ref_count;
  T         value;
  BoxedValue()    : ref_count(1) {}
  BoxedValue(T x) : ref_count(1), value(x) {}
};

template <typename T>
constexpr
bool hasRefs() { return std::is_base_of<HasRefs,T>::value; }


// Relese this reference to the box.
template <typename T>
void free_boxed(BoxedValue<T> *ptr) {
  RefCount n = ptr->ref_count;
  if (n == 1) {
    if constexpr (hasRefs<T>()) ptr->value.free();
    debug("  freeing boxed "); debugValNL((void*) ptr);
    delete ptr;
  }
  else {
    ptr->ref_count = n - 1;
  }
}

// Relese this reference to the box, and we've already extracted its
// content so we don't free it.
template <typename T>
void shallow_free_boxed(BoxedValue<T> *ptr) {
  RefCount n = ptr->ref_count;
  if (n == 1) delete ptr;
  else ptr->ref_count = n - 1;
}


// Relese this reference to the box.
template <typename T>
inline
void copy_boxed(BoxedValue<T> *ptr) { ++(ptr->ref_count); }



// A reference counted pointer.   Note that reference counting is done manually
// using the `copy` and `free` methods and NOT by
// copying constructors/assignment/destructors.
// This gives us more precise control over the life times of things.
// Note that there is no way to check if a reference is "dangling".
template <typename T>
class Boxed : IsBoxed {

  BoxedValue<T> *ptr;

public:
  Boxed()    : ptr(NULL) {}
  Boxed(T x) : ptr (new BoxedValue<T>(x)) {
    debug("  new boxed "); debugValNL((void*) ptr);
  }

  bool isNull() { return ptr == NULL; }

  RefCount refCount() { return ptr->ref_count; }

  // Allocate without initializing the data, but ref count is 1
  void allocate() { ptr = new BoxedValue<T>(); }

  // Release the memory for an object that has already been unitialized.
  void del() { delete ptr; }

  // Relese this reference to the box.
  void free() { free_boxed(ptr); }

  // Relese this reference to the box, without freeing the content.
  void shallowFree() { shallow_free_boxed(ptr); }

  // Make a new "owned" copy of the referece (i.e., increase ref count).
  void copy() { copy_boxed(ptr); }

  // Get access to the contents of the box.
  // The resulting reference shouldn't be used after the box is gone.
  // Borrows the value
  T& getValue() { return ptr->value; }

  bool operator == (Boxed x) { return getValue() == x.getValue(); }
  bool operator != (Boxed x) { return getValue() != x.getValue(); }

  // For debugging
  BoxedValue<T> *rawPtr() { return ptr; }
  void dump() {
    debugVal((void*)ptr);
    debug(" ("); debugVal(ptr? refCount() : 0); debugVal("NL)");
  }
};

}


#endif

