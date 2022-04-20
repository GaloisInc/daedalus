#include "ascii85.hpp"
#include <iostream>

bool ASCII85Decode(const char *text, size_t n, std::vector<uint8_t> &buffer)
{
  uint32_t next = 0;
  int counter = 0;

  buffer.reserve(n); // overapproximated

  for (size_t i = 0; i < n; i++) {
    char const x = text[i];

    if ('!' <= x && x <= 'u') {
      next = next * 85 + uint32_t(x-33);
      counter++;

      if (counter == 5) {
        buffer.push_back(next >> (3*8));
        buffer.push_back(next >> (2*8));
        buffer.push_back(next >> (1*8));
        buffer.push_back(next >> (0*8));
        next = 0;
        counter = 0;
      }
    } else if (x == '~' && i+1 < n && '>' == text[i+1] && counter != 1) {
      if (counter > 1) {
        for (int j = counter; j < 5; j++) {
            next = next * 85 + ('u'-33);
        }

                         buffer.push_back(next >> (3*8));
        if (counter > 2) buffer.push_back(next >> (2*8));
        if (counter > 3) buffer.push_back(next >> (1*8));
      }

      return true;
    } else if (x == 'z' && counter == 0) {
      buffer.push_back(0);
      buffer.push_back(0);
      buffer.push_back(0);
      buffer.push_back(0);
    } else if (!isspace(x)) {
      return false;
    }
  }

  return false;
}
