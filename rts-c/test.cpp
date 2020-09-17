#include <string.h>
#include <memory>
#include <gmpxx.h>
#include "parser.h"
#include "unit.h"
#include "number.h"
#include "closure.h"
#include "iterator.h"


class ThisThunk : public Closure {
  int x;
public:
  ThisThunk(int x) : x(x) { std::cout << "create ThisThunk" << std::endl; }
  ~ThisThunk() { std::cout << "destroy ThisThunk" << std::endl; }
  void enter() { std::cout << "enter ThisThunk: " << x << std::endl; }
};

DataStack stack;

int main() {
  mpz_class a = 2;
  mpz_class b = 3;
  std::cout << a + b << std::endl;

  return 0;
}
