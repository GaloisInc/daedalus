#pragma once

/*
Helpers for exporting Daedalus values to various C++ types.
*/

#include <string>
#include <vector>

class StringBuilder {
public:
  using T = std::string;
private:
  T s;
public:
  void start(size_t n) { s.reserve(n); }
  void push(char x) { s.push_back(x); }
  T done() { return std::move(s); }
};

template <typename E>
class VectorBuilder {
public:
  using T = std::vector<E>;
private:
  T v;
public:
  void start(size_t n) { v.reserve(n); }
  void push(E &&x) { v.push_back(std::move(x)); }
  T done() { return std::move(v); }
};

// Returns a pointer to the rest of the buffer and how much space is remaining.
template <typename E>
class BufferWriter {
    E* current;
    E* terminator;
public:
  using T = std::pair<size_t,E*>;
  BufferWriter(size_t space, E* buffer): current(buffer), terminator(buffer + space) {}
  void start(size_t n) { assert((terminator - current) >= n); }
  void push(E &&x) { *current++ = std::move(x); }
  T done() { return { terminator - current, current }; }
};