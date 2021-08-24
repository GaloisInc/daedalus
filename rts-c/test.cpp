#include "ddl/unit.h"
#include "ddl/number.h"
#include "ddl/list.h"
#include "ddl/array.h"
#include "ddl/integer.h"
#include "ddl/boxed.h"
#include "ddl/cast.h"
#include "ddl/map.h"
#include "ddl/size.h"


#include "user_data.h"

#include "test_size.cpp"
#include "test_map.cpp"
#include "test_numbers.cpp"
#include "test_integer.cpp"
#include "test_casts.cpp"

using namespace std;



int main() {
  test_casts();
  return 0;
}
