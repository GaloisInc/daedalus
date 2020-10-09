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

using IntList = DDL::List<DDL::Integer>;

int main() {

  DDL::Integer x("123");
  x.copy();
  IntList xs(x,IntList());
  IntList ys(x,xs);

  DDL::Array<DDL::Integer> arr(ys);

  DDL::Integer i = arr[0];
  cout << arr.size() << endl;
  cout << i << endl;
  arr.free();


  return 0;
}
