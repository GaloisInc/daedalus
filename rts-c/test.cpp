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
  IntList ys(x,xs); //gives up x

  DDL::Array<DDL::Integer> arr(ys); // give up ys
  arr.copy();

  cout << x.refCount() << endl;

  DDL::Array<DDL::Array<DDL::Integer>> arr2(2,arr,arr); // give up arr
  cout << x.refCount() << endl;

  DDL::Array<DDL::Integer> arr3(arr2);
  arr2.free();

  cout << x.refCount() << endl;

  cout << arr3.refCount() << endl;
  arr3.free();

  return 0;
}
