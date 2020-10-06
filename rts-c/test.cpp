#include <string.h>
#include <memory>
#include <gmpxx.h>
#include <unordered_map>

#include "ddl/parser.h"
#include "ddl/unit.h"
#include "ddl/number.h"
#include "ddl/closure.h"
#include "ddl/array.h"
#include "ddl/integer.h"
#include "ddl/boxed.h"

using namespace std;


int main() {

  DDL::Integer x("123");
  DDL::Integer y(x.copy());
  cout << DDL::add<1>(x,y) << endl;
  cout << x << endl;
  cout << y << endl;
  return 0;
}
