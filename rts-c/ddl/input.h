#ifndef DDL_INPUT_H
#define DDL_INPUT_H

#include <cstdint>
#include <string.h>

#include <ddl/debug.h>
#include <ddl/boxed.h>
#include <ddl/array.h>
#include <ddl/number.h>
#include <ddl/integer.h>

namespace DDL {

class Input;

int compare(Input x, Input y);
std::ostream& toJS(std::ostream& os, Input x);

class Input : HasRefs {
  Array<UInt<8>> name;        // Name identifying the input (e.g , file name)
  Array<UInt<8>> bytes;       // Bytes for the whole input
  size_t offset;              // Offset of next character, if any
  size_t last_offset;         // Offset of end-of-input (1 past last char.)

  // INV: offset <= bytes.size()
  // INV: last_offset <= bytes_len

public:
  Input ()
    : name(), bytes(), offset(0), last_offset(0) {}

  // Owns arguments
  Input(Array<UInt<8>> name, Array<UInt<8>> bytes)
    : name(name), bytes(bytes), offset(0), last_offset(bytes.size()) {}

  // Borrows arguments (i.e., we copy them)
  Input (const char *nm, const char *by)
    : name(Array<UInt<8>>((UInt<8>*)nm, strlen(nm) + 1))
    , bytes(Array<UInt<8>>((UInt<8>*)by, strlen(by) + 1))
    , offset(0)
    , last_offset(bytes.size())
  {}

  // Borrows arguments (i.e., we copy them)
  Input(const char *nm, const char *by, size_t len)
    : name(Array<UInt<8>>((UInt<8>*)nm, strlen(nm) + 1))
    , bytes(Array<UInt<8>>((UInt<8>*)by, len))
    , offset(0)
    , last_offset(len)
    {}

  void dumpInput() {
    debug("[");   debugVal((void*) name.borrowData());
    debug(",");   debugVal((void*) bytes.borrowData());
    debug("] ("); debugVal(name.refCount());
    debug(",");   debugVal(bytes.refCount());
    debugLine(")\n");
  }

  void copy() {
    debug("  Incrementing input"); dumpInput();
    name.copy(); bytes.copy();
  }

  void free() {
    debug("  Decrementing input: "); dumpInput();
    name.free(); bytes.free();
  }


  // borrow this
  size_t  getOffset() { return offset; }
  size_t  length()    { return last_offset - offset; }
  bool    isEmpty()   { return last_offset == offset; }

  // borrow this, Assumes: !isEmpty()
  UInt<8> iHead()   { return bytes[Size{offset}]; }

  // Advance current location
  // Mutates
  // Assumes: n <= length()
  void    iDropMut(Size n)     { offset += n.rep(); }

  // Restrict amount of input
  // Mutates
  // Assumes: n <= length()
  void    iTakeMut(Size n)     { last_offset = offset + n.rep(); }

  // Advance current location
  // Assumes: n <= length()
  // Owns this
  // Since we own *this* the copy constructor does not need to adjust
  // counts:  we are destroyed, but a new copy is returned so counts are
  // preserved
  Input iDrop(Size n)     { Input x(*this); x.iDropMut(n); return x; }

  // Restrict amount of input
  // Assumes: n <= length()
  // Owns this.
  // Since we own *this* the copy constructor does not need to adjust
  // counts:  we are destroyed, but a new copy is returned so counts are
  // preserved
  Input iTake(Size n)     { Input x(*this); x.iTakeMut(n); return x; }

  // Check if the given array of bytes is prefix of the current input.
  bool hasPrefix(DDL::Array<UInt<8>> pref) {
    size_t n = pref.size();
    if (length() < n) return false;
    for (size_t i = 0; i < n; ++i) {
      if (bytes[Size{offset + i}] != pref[Size{i}]) return false;
    }
    return true;
  }

  // XXX: We need to esacpe quotes in the input name
  friend
  std::ostream& operator<<(std::ostream& os, Input x) {
    os << "Input(\"" << x.name.refCount() << "," << x.bytes.refCount()
                     << "," << (char*)x.name.borrowData()
                   << ":0x" << std::hex << x.offset << "--0x"
                            << std::hex << x.last_offset << "\")";

     return os;
  }

  // XXX: We need to esacpe quotes in the input name
  friend
  std::ostream& toJS(std::ostream& os, Input x) {
    os << "{ \"$$input\": \"" << (char*)x.name.borrowData()
                   << ":0x" << std::hex << x.offset << "--0x"
                            << std::hex << x.last_offset << "\"}";

    return os;
  }

  // we compare by name, not the actual byte content
  friend
  int compare(Input x, Input y) {
    if (x.offset < y.offset) return -1;
    if (x.offset > y.offset) return 1;
    if (x.last_offset < y.last_offset) return -1;
    if (x.last_offset > y.last_offset) return 1;
    return compare(x.name,y.name);
  }

  char* borrowBytes() { return (char*) bytes.borrowData() + offset; }
};



// Borrow arguments
static inline
bool operator == (Input xs, Input ys) { return compare(xs,ys) == 0; }

// Borrow arguments
static inline
bool operator < (Input xs, Input ys) { return compare(xs,ys) < 0; }

// Borrow arguments
static inline
bool operator > (Input xs, Input ys) { return compare(xs,ys) > 0; }

// Borrow arguments
static inline
bool operator != (Input xs, Input ys) { return !(xs == ys); }

// Borrow arguments
static inline
bool operator <= (Input xs, Input ys) { return !(xs > ys); }

// Borrow arguments
static inline
bool operator >= (Input xs, Input ys) { return !(xs < ys); }





}

#endif
