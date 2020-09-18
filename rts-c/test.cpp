#include <string.h>
#include <memory>
#include <gmpxx.h>
#include "parser.h"
#include "unit.h"
#include "number.h"
#include "closure.h"
#include "iterator.h"
#include "array.h"


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
  //

  DDL::ArrayBuilder<int> xs = NULL;
  xs = new DDL::ArrayCons<int>(1,xs);
  xs = new DDL::ArrayCons<int>(2,xs);
  xs = new DDL::ArrayCons<int>(3,xs);

  DDL::Array<int> arr;
  DDL::buildArray(xs,arr);
  std::cout << "BUILT" << std::endl;
  // arr = DDL::Array<int>();

  DDL::Array<int> arr2 = arr;
  std::cout << "COPIED" << std::endl;

  std::cout << arr2.size() << std::endl;

  DDL::ArrayIterator<int> it(arr);
  while(!it.done()) {
    std::cout << it.key() << " = " << it.value() << std::endl;
    it = it.next();
  }

  return 0;
}
