#ifndef DDL_DEBUG
#define DDL_DEBUG

#define DEBUG_LEVEL 0

#if DEBUG_LEVEL > 0
#include <iostream>
#endif

namespace DDL {

static inline
void debug(const char* msg) {
#if DEBUG_LEVEL > 0
  std::cerr << msg;
#endif
}

static inline
void debugNL() {
#if DEBUG_LEVEL > 0
  std::cerr << std::endl;
#endif
}

template <typename T>
static inline
void debugVal(const T& val) {
#if DEBUG_LEVEL > 0
  std::cerr << val;
#endif
}

static inline
void debugLine(const char* msg) {
  debug(msg);
  debugNL();
}

template <typename T>
static inline
void debugValNL(const T& val) {
  debugVal<T>(val);
  debugNL();
}

}

#endif
