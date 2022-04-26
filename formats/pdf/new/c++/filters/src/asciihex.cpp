#include "asciihex.hpp"

namespace {
  // 0-15 digit value
  // -1 end of data
  // -2 whitespace
  // -3 error
  int asciiHexTable[256] = {
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-2,-2,-2,-2,-2,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -2,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
       0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-3,-3,-3,-3,-1,-3,
      -3,10,11,12,13,14,15,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,10,11,12,13,14,15,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -2,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,
      -3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3};
}

bool ASCIIHexDecode(char const* text, size_t n, std::vector<uint8_t> &buffer)
{
  buffer.reserve(n / 2);

  int prev;
  bool half = false;

  for (size_t i = 0; i < n; i++) {
    int x = asciiHexTable[text[i]];
    switch(x) {
      default:
        if (half) {
          buffer.push_back(prev<<4 | x);
          half = false;
        } else {
          prev = x;
          half = true;
        }
        break;
      case -1: // end of data
        if (half) {
          buffer.push_back(prev<<4);
        }
        return true;
      case -2: // whitespace
        break;
      case -3: // error
        return false;
    }
  }

  // No end of data found
  return false;
}
