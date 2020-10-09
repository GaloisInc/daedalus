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
  C xs = { .xs = DDL::Array<int>(2,7,8)
         , .ys = DDL::Array<DDL::Integer>(1,x)
         };

  xs.free();

  return 0;
}
