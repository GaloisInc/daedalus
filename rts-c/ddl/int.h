#error "XXX: Small int is out of date"

#include <climits>
#include <stdlib.h>
#include <ddl/boxed.h>

namespace DDL {
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


static inline
int compare(Integer x, Integer y) {
  long a = x.asSLong();
  long b = y.asSLong();
  return (a < b) ? -1 : (a - b);
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

static inline
std::ostream& toJS(std::ostream& os, Integer x) {
  return os << std::dec << x.asSLong();
}

// NOTE: lcat is in `number.h` to avoid dependency convlicts
}


