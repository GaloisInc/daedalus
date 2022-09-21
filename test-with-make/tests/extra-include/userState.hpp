#ifndef USER_STATE_HPP
#define USER_STATE_HPP

#include <ddl/number.h>

class UserState {
  DDL::UInt<8> x;
public:
  UserState() : x(0) {}

  void         increment() { x = x + 1; }
  DDL::UInt<8> get()       { return x; }
};


#endif
