#ifndef DDL_CLOSURE
#define DDL_CLOSURE

#include <vector>
#include <memory>


namespace DDL {

struct Closure {
  virtual void enter() = 0;
};

using DataStack = std::vector<std::shared_ptr<Closure>>;

template < typename T, typename... Args >
static inline
void stackPush( DataStack& stack, Args&&... args ) {
  stack.push_back( std::static_pointer_cast<Closure>(
                   std::make_shared<T>(args...)
                 ));
}

}

#endif
