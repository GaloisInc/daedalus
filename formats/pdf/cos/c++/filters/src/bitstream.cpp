#include "bitstream.hpp"

#include <algorithm>

BitStream::BitStream(uint8_t const* ptr, size_t size)
: ptr(ptr), remain(size), current(0), bits(0) {}

int
BitStream::get(int want) {
    int result = 0;

    while (want > 0) {
        if (bits == 0) {
            if (remain == 0) {
                return -1;
            }
            current = *ptr++;
            remain--;
            bits = 8;
        }

        int amount = std::min(want, bits);
        want -= amount;

        result <<= amount;
        if (amount == 8) {
            result |= current;
            bits = 0;
        } else {
            result |= current >> (8-amount);
            current <<= amount;
            bits -= amount;
        }
    }

    return result;
}
