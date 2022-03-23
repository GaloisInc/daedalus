#ifndef DEBUG_HPP
#define DEBUG_HPP

#define DEBUG

#include <iostream>
#ifdef DEBUG
#define dbg std::cerr
#else
extern std::ostream dbg;

#endif
#endif
