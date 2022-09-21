#pragma once

// #define DEBUG

#include <iostream>
#ifdef DEBUG
#define dbg std::cerr
#else
extern std::ostream dbg;

#endif
