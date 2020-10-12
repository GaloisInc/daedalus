#include <string.h>
#include <memory>
#include <gmpxx.h>
#include <unordered_map>
#include <type_traits>

// #include "ddl/parser.h"
#include "ddl/unit.h"
#include "ddl/number.h"
#include "ddl/closure.h"
#include "ddl/list.h"
#include "ddl/array.h"
#include "ddl/integer.h"
#include "ddl/boxed.h"


using namespace std;


struct C : public DDL::HasRefs {
  DDL::Array<int> xs;
  DDL::Array<DDL::Integer> ys;

  void free() { xs.free(); ys.free(); }
};

int main() {
  DDL::Integer x("123");
  x.copy();
  DDL::Array<DDL::Integer> xs(2,x,x);     // give up x
  DDL::Array<DDL::Integer>::Iterator it(xs);  // give up xs

  while (! it.done()) {
    cout << it.borrowValue() << endl;
    it = it.next();   // give up it, and get new it
  }
  it.free();      // give up it

  return 0;
}
