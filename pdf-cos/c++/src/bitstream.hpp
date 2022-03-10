#pragma once

#include <cstdlib>

class BitStream
{
    uint8_t const* ptr;
    size_t remain;
    uint8_t current;
    int bits;

public:
    BitStream(uint8_t const*, size_t);
    BitStream() = delete;

    int get(int bits);
};
