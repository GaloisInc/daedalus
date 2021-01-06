#ifndef DDL_INPUT_H
#define DDL_INPUT_H

#include <cstdint>
#include <string.h>

#include <ddl/boxed.h>
#include <ddl/array.h>
#include <ddl/number.h>
#include <ddl/integer.h>

namespace DDL {

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
    : name(Array<UInt<8>>((UInt<8>*)nm, strlen(nm)))
    , bytes(Array<UInt<8>>((UInt<8>*)by, strlen(by)))
    , offset(0)
    , last_offset(bytes.size())
  {}

  // Borrows arguments (i.e., we copy them)
  Input(const char *nm, const char *by, size_t len)
    : name(Array<UInt<8>>((UInt<8>*)nm, strlen(nm)))
    , bytes(Array<UInt<8>>((UInt<8>*)by, len))
    , offset(0)
    , last_offset(len)
    {}

  void copy() { name.copy(); bytes.copy(); }
  void free() { name.free(); bytes.free(); }


  size_t  getOffset() { return offset; }
  size_t  length()    { return last_offset - offset; }
  bool    isEmpty()   { return last_offset == offset; }

  // Assumes: !isEmpty()
  UInt<8> iHead()   { return bytes[offset]; }

  // Advance current location
  // Mutates
  // Assumes: n <= length()
  void    iDropMut(size_t n) { offset += n; }

  // Restrict amount of input
  // Mutates
  // Assumes: n <= length()
  void    iTakeMut(size_t n) { last_offset = offset + n; }

  Input iDropI(DDL::Integer n) { return iDrop(n.asULong()); }
  Input iTakeI(DDL::Integer n) { return iTake(n.asULong()); }


  // Advance current location
  // Assumes: n <= length()
  // Owns this
  // Since we own *this* the copy constructor does not need to adjust
  // counts:  we are destroyed, but a new copy is returned so counts are
  // preserved
  Input iDrop(size_t n) { Input x(*this); x.iDropMut(n); return x; }

  // Restrict amount of input
  // Assumes: n <= length()
  // Owns this.
  // Since we own *this* the copy constructor does not need to adjust
  // counts:  we are destroyed, but a new copy is returned so counts are
  // preserved
  Input iTake(size_t n) { Input x(*this); x.iTakeMut(n); return x; }

  // Check if the given array of bytes is prefix of the current input.
  bool hasPrefix(DDL::Array<UInt<8>> pref) {
    size_t n = pref.size();
    if (length() < n) return false;
    for (size_t i = 0; i < n; ++i) {
      if (bytes[offset + i] != pref[i]) return false;
    }
    return true;
  }

  bool operator == (Input x) {
    return offset == x.offset &&
           last_offset == x.last_offset &&
           name == x.name;
           // if we want to compare bytes, we should hash them
  }

  bool operator != (Input x) { return !(*this == x); }

  // XXX: We need to esacpe quotes in the input name
  friend
  std::ostream& operator<<(std::ostream& os, Input x) {
    os << "Input(\"" << (char*)x.name.borrowData()
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




};

// XXX: comparisions




}

#endif
