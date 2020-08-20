#ifndef DDL_INPUT_H
#define DDL_INPUT_H

#include <cstdint>
#include <cstring>

class Input {
  const char *name;           // Name identifying the input (e.g , file name)
  const char *bytes;          // Bytes for the whole input
  size_t bytes_len;           // Length of `bytes`
  size_t offset;              // Offset of next character, if any
  size_t last_offset;         // Offset of end-of-input (1 past last char.)

  // INV: offset <= last_offset
  // INV: last_offset <= bytes_len

public:
  Input( const char *name     // 0 terminated
       , const char *bytes
       , size_t len
       ) : name(name), bytes(bytes), bytes_len(len),
           offset(0), last_offset(len)
        {}

  bool operator == (Input &i) {
    return
      offset == i.offset &&
      last_offset == i.last_offset &&
      strcmp(name, i.name);
      // XXX: we could add a hash of the bytes, also for now we
      // use the name as a proxy.
  }


  size_t  getOffset() { return offset; }
  size_t  length()    { return last_offset - offset; }
  bool    isEmpty()   { return last_offset == offset; }

  // Assumes: !isEmpty()
  uint8_t iHead()   { return bytes[offset]; }

  // Advance current location
  // Mutates
  // Assumes: n <= length()
  void    iDrop(size_t n) { offset += n; }

  // Restrict amount of input
  // Mutates
  // Assumes: n <= length()
  void    iTake(size_t n) { last_offset = offset + n; }

  // Check if the given array of bytes is prefix of the current input.
  bool hasPrefix(std::vector<uint8_t> pref) {
    size_t n = pref.size();
    if (length() < n) return false;
    for (size_t i = 0; i < n; ++i) {
      if (bytes[offset + i] != pref[i]) return false;
    }
    return true;
  }
};

#endif
