#include "lzw.hpp"

#include <iostream>

int main()
{
    uint8_t abexample[] = {0x80, 0x0b, 0x60, 0x50, 0x22, 0x0c, 0x0c, 0x85, 0x01};
    std::cout << decompress(abexample, std::size(abexample)) << std::endl;
}
