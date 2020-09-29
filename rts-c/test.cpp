#include <string.h>
#include <memory>
#include <gmpxx.h>
#include "parser.h"
#include "unit.h"
#include "number.h"
#include "closure.h"
#include "array.h"
#include <unordered_map>
#include "integer.h"


class ThisThunk : public Closure {
  int x;
public:
  ThisThunk(int x) : x(x) { std::cout << "create ThisThunk" << std::endl; }
  ~ThisThunk() { std::cout << "destroy ThisThunk" << std::endl; }
  void enter() { std::cout << "enter ThisThunk: " << x << std::endl; }
};

DataStack stack;

int main() {
  DDL::Integer x("123");
  DDL::Integer y("756");
  std::cout << x*y << std::endl;
  return 0;
}
