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

#include "user_data.h"

using namespace std;


int main() {
  List nil;
  nil.init_Nil();

  Node n;
  n.init(17,nil);

  List one;
  one.init_Cons(n);

  cout << (one == one) << endl;
  return 0;
}
