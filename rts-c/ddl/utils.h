#ifndef DDL_UTILS_H
#define DDL_UTILS_H

#include <vector>
#include <ddl/parser.h>

namespace DDL {

template <typename T, typename... Args>
inline
bool parseOne
  ( void (*f)(DDL::ParseError&, std::vector<T>&, Args...)
  , DDL::ParseError error
  , T* out
  , Args... args
  ) {
  std::vector<T> results;
  f(error, results, args...);
  if (results.size() != 1) {
    for (auto && x : results) x.free();
    return false;
  }
  *out = results[0];
  return true;
}

}

#endif
