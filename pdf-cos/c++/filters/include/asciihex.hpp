#pragma once

#include <vector>
#include <cstdint>

bool ASCIIHexDecode(char const* text, size_t n, std::vector<uint8_t> &buffer);
