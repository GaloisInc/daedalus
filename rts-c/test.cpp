#include <string.h>
#include <memory>
#include "parser.h"
#include "unit.h"
#include "number.h"
#include "closure.h"


class ThisThunk : public Closure {
  int x;
public:
  ThisThunk(int x) : x(x) { std::cout << "create ThisThunk" << std::endl; }
  ~ThisThunk() { std::cout << "destroy ThisThunk" << std::endl; }
  void enter() { std::cout << "enter ThisThunk: " << x << std::endl; }
};

DataStack stack;

int main() {
  stackPush<ThisThunk>(stack,1);
  return 0;
}
