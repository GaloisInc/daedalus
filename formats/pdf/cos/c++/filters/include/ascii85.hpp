#pragma once
#include <vector>
#include <cstdint>
#include <cstdlib>

bool ASCII85Decode(const char *text, size_t n, std::vector<uint8_t> &buffer);
