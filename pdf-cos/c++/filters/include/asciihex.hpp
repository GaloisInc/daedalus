#pragma once

#include <vector>
#include <cstdint>
#include <cstdlib>

bool ASCIIHexDecode(char const* text, size_t n, std::vector<uint8_t> &buffer);
