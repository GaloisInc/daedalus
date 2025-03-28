#ifndef DDL_INPUT_H
#define DDL_INPUT_H

#include <cstdint>
#include <cassert>
#include <string.h>

#include <ddl/debug.h>
#include <ddl/boxed.h>
#include <ddl/array.h>
#include <ddl/number.h>
#include <ddl/integer.h>
#include <ddl/maybe.h>

namespace DDL {

class Input;

int compare(Input x, Input y);
std::ostream& toJS(std::ostream& os, Input x);

class Input : HasRefs {
  Array<UInt<8>> name;        // Name identifying the input (e.g , file name)
  Array<UInt<8>> bytes;       // Bytes for the whole input
  Size offset;                // Offset of next character, if any
  Size last_offset;           // Offset of end-of-input (1 past last char.)

  // INV: offset <= bytes.size()
  // INV: last_offset <= bytes_len

public:
  Input ()
    : name(), bytes(), offset(Size{0}), last_offset(Size{0}) {}

  // Owns arguments
  Input(Array<UInt<8>> name, Array<UInt<8>> bytes)
    : name(name), bytes(bytes), offset(Size{0}), last_offset(bytes.size()) {}

  // Borrows arguments (i.e., we copy them)
  // Note that the 0 terminators are NOT stored in the input
  Input (const char *nm, const char *by)
    : name(Array<UInt<8>>((UInt<8>*)nm, Size::from(strlen(nm))))
    , bytes(Array<UInt<8>>((UInt<8>*)by, Size::from(strlen(by))))
    , offset(Size{0})
    , last_offset(bytes.size())
  {}

  // Borrows arguments (i.e., we copy them)
  // Note that the 0 terminator on the name is NOT stored in the input
  Input(const char *nm, const char *by, Size len)
    : name(Array<UInt<8>>((UInt<8>*)nm, Size::from(strlen(nm))))
    , bytes(Array<UInt<8>>((UInt<8>*)by, len))
    , offset(Size{0})
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
  Size    getOffset() const { return offset; }
  Size    length()    const { return Size{last_offset.rep() - offset.rep()}; }
  bool    isEmpty()   const { return last_offset == offset; }

  // borrow this, Assumes: !isEmpty()
  UInt<8> iHead() const { return bytes[offset]; }

  // Advance current location
  // Mutates
  // Assumes: n <= length()
  void    iDropMut(Size n)     { assert(n <= length()); offset.incrementBy(n); }

  // Restrict amount of input
  // Mutates
  void    iTakeMut(Size n)     {
    auto n1 = length();
    if (n > n1) n = n1;
    last_offset = offset.incrementedBy(n);
  }

  // Advance current location
  // Assumes: n <= length()
  // Owns this
  // Since we own *this* the copy constructor does not need to adjust
  // counts:  we are destroyed, but a new copy is returned so counts are
  // preserved
  Input iDrop(Size n)     { Input x(*this); x.iDropMut(n); return x; }

  Maybe<Input> iDropMaybe(Size n) {
    if (n > length()) return Maybe<Input>();
    return Maybe(iDrop(n));
  }

  // Restrict amount of input
  // Owns this.
  // Since we own *this* the copy constructor does not need to adjust
  // counts:  we are destroyed, but a new copy is returned so counts are
  // preserved
  Input iTake(Size n)     { Input x(*this); x.iTakeMut(n); return x; }

  // Check if the given array of bytes is prefix of the current input.
  bool hasPrefix(DDL::Array<UInt<8>> pref) {
    Size n = pref.size();
    if (length() < n) return false;
    for (Size i = Size{0}; i < n; i.increment()) {
      if (bytes[offset.incrementedBy(i)] != pref[i]) return false;
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

  Array<UInt<8>> getByteArray() {
    auto len = length();
    if (bytes.size() == len) {
      bytes.copy(); return bytes;
    }

    return Array{bytes.borrowData() + offset.rep(), len};
  }

  Array<UInt<8>> getName()          { name.copy(); return name; }
  Array<UInt<8>> borrowName() const { return name; }
  std::string_view borrowNameBytes() const { return name.borrowBytes(); }
  std::string_view borrowBytes() const {
    return bytes.borrowBytes(offset, last_offset.decrementedBy(offset));
  }
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
