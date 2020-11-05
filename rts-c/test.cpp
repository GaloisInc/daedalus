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

using namespace std;


int main() {

  // example();

  DDL::UInt<8> x(3);
  cout << x << endl << DDL::uint_to_uint<8,16>(x) << endl;
  cout << DDL::uint_to_uint_maybe<8,4>(x) << endl;
  cout << DDL::uint_to_integer<8>(x) << endl;

  DDL::Integer i = DDL::uint_to_integer<64>(125);
  cout << DDL::integer_to_uint_maybe<8>(i) << endl;



  return 0;
}
