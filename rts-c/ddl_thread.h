#ifndef DDL_DDL_THREAD
#define DDL_DDL_THREAD

#include <vector>
#include "closure.h"

class Thread {
  Closure1<bool> *closure;
  bool            notified;
public:

  Thread(Closure1<bool> *c)
    : closure(c)
    , notified(false)
  {}

  void            notify()     { notified = true; }
  Closure1<bool> *getClosure() { return closure; }
};

#endif
