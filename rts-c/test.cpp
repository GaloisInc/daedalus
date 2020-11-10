#include <string.h>
#include <memory>
#include <gmpxx.h>
#include <unordered_map>
#include <type_traits>

// #include "ddl/parser.h"
#include "ddl/unit.h"
#include "ddl/number.h"
#include "ddl/list.h"
#include "ddl/array.h"
#include "ddl/integer.h"
#include "ddl/boxed.h"
#include "ddl/cast.h"

#include "user_data.h"
#include "fun_call.h"
#include "test.h"

using namespace std;




int main() {

  // example();
  // cast_tests();
  rb_tests();

  return 0;
}
