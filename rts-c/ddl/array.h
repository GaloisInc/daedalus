#ifndef DDL_ARRAY_H
#define DDL_ARRAY_H

#include <string.h>
#include <cctype>
#include <algorithm>
#include <functional>
#include <vector>
#include <string_view>

#include <ddl/debug.h>
#include <ddl/list.h>
#include <ddl/number.h>
#include <ddl/size.h>

namespace DDL {

template <typename T> class Builder;
class Stream;

template <typename T>
class Array : IsBoxed {

  class Content {
    friend Array;
    RefCount  ref_count;
    Size      size;
    T         data[];


  public:
    friend Builder<T>;
    friend Stream;

    // Allocate an array with unitialized data
    static
    Content *allocate(Size n) {
      size_t hdr_size = sizeof(Content);
      size_t need_bytes = hdr_size + n.rep() * sizeof(T);
      // In unites of content, so that we can get proper alignment
      size_t need_content = (need_bytes - 1 + hdr_size) / hdr_size;
      Content *p   = new Content[need_content];
      p->ref_count = 1;
      p->size      = n;
      return p;
    }

  } *ptr;

  Array(Content *p) : ptr(p) {}

  static
  Size rangeSize(Size space, Size step) {
    Size ents  = Size{space.rep() / step.rep()};
    if (space.rep() % step.rep() > 0) ents.increment();
    return ents;
  }

public:
  friend Builder<T>;
  friend Stream;

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

  Array() : ptr(nullptr) {}

  Array(std::initializer_list<T> xs)
  : ptr(Content::allocate(xs.size())) {
    std::copy(xs.begin(), xs.end(), ptr->data);
  }

  // Array from builder. Owns b
  Array(Builder<T> b) : ptr(b.toContent()) {}

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
  Array(T const *data, Size n)
  : ptr(Content::allocate(n)) {
    std::copy_n(data, n.rep(), ptr->data);
  }

  // Borrows this
  Size size() const { return ptr == nullptr ? 0 : ptr->size; }

  // Borrow this.
  // Returns a borrowed version of to element (if reference)
  T borrowElement(Size i) const { return ptr->data[i.rep()]; }

  // Borrows this
  // Returns an owned copy of the element.
  T operator[] (Size i0) const {
    size_t i = i0.rep();
    if constexpr (std::is_base_of<HasRefs,T>::value) {
      T& x = ptr->data[i];
      x.copy();
      return x;
    }
    return ptr->data[i];
  }


  T* borrowData() const {
    if(ptr == nullptr) {
      return nullptr;
    }
    return (T*)&ptr->data;
  }

  std::string_view borrowBytes() const { return borrowBytes(Size(0),size()); }

  std::string_view borrowBytes(Size from) const {
    return borrowBytes(from, size().decrementedBy(from));
  }

  // Get a view of the bytes starting at given offset and of the given length.
  std::string_view borrowBytes(Size from, Size len) const {
    static_assert( std::is_same_v<T, UInt<8>>
                || std::is_same_v<T, uint8_t>
                || std::is_same_v<T, char>
                 , "borrowBytes works only for UInt<8>, uint8_t, and char");
    assert(from.incrementedBy(len) <= size());
    return std::string_view( reinterpret_cast<char const*>(borrowData()) +
                             from.rep()
                           , len.rep()
                           );
  }


  /// Owned this
  template <typename ExtArrayBuilder, typename ExportElement>
  typename ExtArrayBuilder::T export_array(ExtArrayBuilder &&builder, ExportElement elExp) {
    auto n = size().rep();
    builder.start(n);
    auto r = refCount();
    if (r == 1) {
      for (size_t i = 0; i < n; ++i) {
        builder.push(elExp(borrowElement(i)));
      }
      delete ptr;
      ptr = nullptr;
    } else {
      for (size_t i = 0; i < n; ++i) {
        builder.push(elExp((*this)[i]));
      }
      ptr->ref_count = r - 1;
    }
    return builder.done();
  }





// -- Boxed --------------------------------------------------------------------
  RefCount refCount() { return ptr == nullptr ? 0 : ptr->ref_count; }

  void copy() { if (ptr != nullptr) ptr->ref_count++; }

  void free() {
    if (ptr == nullptr) return;

    RefCount n = refCount();
    if (n == 1) {
      if constexpr (std::is_base_of<HasRefs,T>::value) {
        size_t todo = ptr->size.rep();
        T* arr = ptr->data;
        for(size_t i = 0; i < todo; ++i) arr[i].free();
      }
      debug("  Freeing array "); debugValNL((void*)ptr);
      delete[] ptr;
      ptr = nullptr;
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
      return Iterator(index.incremented(), xs);
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

template <typename T>
static inline
int compare (Builder<T> b1, Builder<T> b2);

template <typename T>
class Builder {
    List<std::vector<T>> list;

  public:
    friend int compare<T> (Builder<T> b1, Builder<T> b2);

    Builder () : list() {}

    // owns x, xs
    Builder (Builder xs, T x) {
      if (xs.list.refCount() == 1) {
        list = xs.list;
        list.borrowHead().push_back(x);
      } else {
        list = List{{x}, xs.list};
      }
    }

    // owns a, b
    Builder(Builder b, Array<T> a) {
      auto beginData = a.ptr->data;
      auto endData = beginData + a.ptr->size.value;

      // support copy if array has 1 reference
      if constexpr (hasRefs<T>()) {
        std::for_each(beginData, endData, std::mem_fn(&T::copy));
      }

      if (b.list.refCount() == 1) {
        auto &v = b.list.borrowHead();
        v.insert(v.end(), beginData, endData);
        list = b.list;
      } else {
        list = List{std::vector(beginData, endData), b.list};
      }
      a.free();
    }

    void free() {
      if constexpr (hasRefs<T>()) {
        for (auto cursor = list; cursor.refCount() == 1; cursor = cursor.borrowTail()) {
          for (auto && x : cursor.borrowHead()) {
            x.free();
          }
        }
      }
      list.free();
    }

    void copy() { list.copy(); }

    // owns *this
    typename Array<T>::Content *toContent() {
      size_t n = 0;
      for (auto cursor = list; !cursor.isNull(); cursor = cursor.borrowTail()) {
        n += cursor.borrowHead().size();
      }
      auto ptr = Array<T>::Content::allocate(Size{n});
      
      bool moving = hasRefs<T>();
      for (auto cursor = list; !cursor.isNull(); cursor = cursor.borrowTail()) {
        moving = moving && cursor.refCount() == 1;
        auto &v = cursor.borrowHead();
        for (auto i = v.size(); i > 0; i--) {
          auto &x = v[i-1];
          if (!moving) x.copy();
          ptr->data[--n] = x;
        }
      }
      list.free();
      return ptr;
    }
  };

// borrow
inline
std::ostream& operator<<(std::ostream& os, Array<UInt<8>> x) {
  Size n = x.size();
  if (n == 0) { os << "[]"; return os; }

  Size count_print = 0;
  for (Size i = 0; i < n; i.increment()) {
    if (isprint(x[i].rep())) count_print.increment();
  }

  float perc = float(100 * count_print.rep()) / n.rep();

  if (perc > 75) {
    auto flags = os.flags();
    os << std::hex;
    os << "\"";
    for (Size i = 0; i < n; i.increment()) {
      uint8_t c = x[i].rep();
      if (c == '\\') os << '\\'; else
      if (isprint(c)) os << (char)c;
      else {
        os << "\\";
        if (c < 16) os << '0';
        os << (int)c;
      }
    }
    os << "\"";
    os.setf(flags);
  }
  else {
    os << "[";
    char sep[] = ", ";
    sep[0] = 0;
    for (Size i = 0; i < n; i.increment()) {
      os << sep << x.borrowElement(i);
      sep[0] = ',';
    }
    os << "]";
  }

  return os;
}



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
std::ostream& operator<<(std::ostream& os, Builder<T> b) {
  b.copy();
  Array<T> x {b};
  Size n = x.size();

  os << "[";
  char sep[] = ", ";
  sep[0] = 0;
  for (Size i = 0; i < n; i.increment()) {
    os << sep << x.borrowElement(i);
    sep[0] = ',';
  }
  os << "]";
  x.free();
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

// borrow
template <typename T>
inline
std::ostream& toJS(std::ostream& os, Builder<T> b) {
  b.copy();
  Array<T> x {b};
  Size n = x.size();

  os << "[";
  char sep[] = ", ";
  sep[0] = 0;
  for (Size i = 0; i < n; i.increment()) {
    toJS(os << sep, x.borrowElement(i));
    sep[0] = ',';
  }
  os << "]";
  x.free();
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
  return size_x == size_y ?  0 :
         size_x  < size_y ? -1 : 1;
}

// borrow arguments
template <typename T>
static inline
int compare (Builder<T> b1, Builder<T> b2) {

  auto c1 = b1.list;
  auto i1 = c1.isNull() ? 0 : c1.borrowHead().size();

  auto c2 = b2.list;
  auto i2 = c2.isNull() ? 0 : c2.borrowHead().size();

  while (i1 && i2) {
    auto r = compare(c1.borrowHead()[--i1], c2.borrowHead()[--i2]);
    if (r) return r;

    if (!i1) {
      c1 = c1.borrowTail();
      if (!c1.isNull()) i1 = c1.borrowHead().size();
    }

    if (!i2) {
      c2 = c2.borrowTail();
      if (!c2.isNull()) i2 = c2.borrowHead().size();
    }
  }

  return i2 ? -1 : !!i1;
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

// Borrow arguments
template <typename T> static inline
bool operator == (Builder<T> xs, Builder<T> ys) { return compare(xs,ys) == 0; }

// Borrow arguments
template <typename T> static inline
bool operator < (Builder<T> xs, Builder<T> ys) { return compare(xs,ys) < 0; }

// Borrow arguments
template <typename T> static inline
bool operator > (Builder<T> xs, Builder<T> ys) { return compare(xs,ys) > 0; }

// Borrow arguments
template <typename T> static inline
bool operator != (Builder<T> xs, Builder<T> ys) { return !(xs == ys); }

// Borrow arguments
template <typename T> static inline
bool operator <= (Builder<T> xs, Builder<T> ys) { return !(xs > ys); }

// Borrow arguments
template <typename T> static inline
bool operator >= (Builder<T> xs, Builder<T> ys) { return !(xs < ys); }







} // namespace DDL
#endif
