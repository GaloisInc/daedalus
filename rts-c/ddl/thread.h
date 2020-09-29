#ifndef DDL_THREAD
#define DDL_THREAD

#include <vector>

#include "ddl/closure.h"

class Thread {
  std::function< void(bool) >             resume;
  bool                                    notified;
  std::vector< std::function< void() > >  stack;
public:

  Thread( std::unique_ptr<Closure1<bool>> c)
    : closure(std::move(c))
    , notified(false)
  {}

  void            notify()     { notified = true; }

  // This gives away the ownership, so it should only be called once.
  // We provide this, so that we can get the closure but destroy the
  // rest of the thread.
  std::unique_ptr<Closure1<bool>> getClosure() { return std::move(closure); }

  bool getNotified() { return notified; }
};

#endif
