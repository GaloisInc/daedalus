#include <climits>
#include <stdlib.h>
#include <ddl/boxed.h>

class Integer {
  long data;

public:
  Integer(): data(0) {}
  Integer(const char* str) : data(atol(str)) {}
  Integer(unsigned long x) : data((long)x) {}
  Integer(long x) : data(x) {}

  bool isNatural() { return data >= 0; }

  // assumes we know things will fit
  unsigned long asULong()    { return (unsigned long) data; }
  long          asSLong()    { return data; }

  bool          fitsULong()  { return 0 <= data; }
  bool          fitsSLong()  { return true; }

  void free() {}
  void copy() {}
};

// borrow
static inline
bool operator == (Integer x, Integer y) { return x.asSLong() == y.asSLong(); }

// borrow
static inline
bool operator != (Integer x, Integer y) { return x.asSLong() != y.asSLong(); }

// borrow
static inline
bool operator <  (Integer x, Integer y) { return x.asSLong() < y.asSLong(); }

// borrow
static inline
bool operator <= (Integer x, Integer y) { return x.asSLong() <= y.asSLong(); }

// borrow
static inline
std::ostream& operator<<(std::ostream& os, Integer x) {
  os << x.asSLong();
  return os;
}

// owned
static inline
Integer operator + (Integer x, Integer y) {
  return Integer(x.asSLong() + y.asSLong());
}

// owned
static inline
Integer operator - (Integer x, Integer y) {
  return Integer(x.asSLong() - y.asSLong());
}

// owned
static inline
Integer operator * (Integer x, Integer y) {
  return Integer(x.asSLong() * y.asSLong());
}

// XXX: Check for division by 0?
// owned
static inline
Integer operator / (Integer x, Integer y) {
  return Integer(x.asSLong() / y.asSLong());
}


// XXX: Check for division by 0?
// owned
static inline
Integer operator % (Integer x, Integer y) {
  return Integer(x.asSLong() % y.asSLong());
}

static inline
// owned
Integer operator - (Integer x) {
  return Integer(-x.asSLong());
}


// owned, borrowed
// XXX: iamt should eventually be replaced with a smaller type
static inline
Integer operator << (Integer x, Integer iamt) {
  return Integer(x.asSLong() << iamt.asULong());
}

// owned, borrowed
// XXX: iamt should eventually be replaced with a smaller type
static inline
Integer operator >> (Integer x, Integer iamt) {
  return Integer(x.asSLong() >> iamt.asULong());
}

// NOTE: lcat is in `number.h` to avoid dependency convlicts



