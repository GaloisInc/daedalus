#ifndef DDL_CLOSURE
#define DDL_CLOSURE

// Suspended computation with no arguments
class Closure0 {
public:
  virtual void enter() = 0;
};


// Suspended computation with 1 argument
template <class T>  // Type of argument
class Closure1 {
public:
  virtual void enter(T x) = 0;
};

#endif
