#include "ddl/unit.h"
#include "ddl/number.h"
#include "ddl/list.h"
#include "ddl/array.h"
#include "ddl/integer.h"
#include "ddl/boxed.h"
#include "ddl/cast.h"
#include "ddl/map.h"

#include "user_data.h"
#include "test.h"

using namespace std;


int main() {
  DDL::Map<DDL::UInt<8>,DDL::UInt<8>> x;
  std::cout << x << std::endl;

  return 0;
}
