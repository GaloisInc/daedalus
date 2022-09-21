#include <pdfcos.hpp>

#ifndef DEBUG

struct NullBuffer : public std::streambuf {
  int overflow(int c) { return c; }
} null_buffer;

std::ostream dbg(&null_buffer);
#endif

