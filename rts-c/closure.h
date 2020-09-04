#ifndef DDL_CLOSURE
#define DDL_CLOSURE

// Suspended computation with no arguments
class Closure0 {
public:
  virtual void operator() () = 0;
  virtual ~Closure0() {}
};


// Suspended computation with 1 argument
template <class T>  // Type of argument
class Closure1 {
public:
  virtual void operator() (T x) = 0;
  virtual ~Closure1() {}
};

#endif
