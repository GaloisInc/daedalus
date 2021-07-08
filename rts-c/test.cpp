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


void compare_test() {
  DDL::Maybe<int> m1{3};
  DDL::Maybe<int> m2{2};
  cout << DDL::Bool(m1 <= m2) << endl;
/*
  DDL::Map<int,int> m;
  m = m.insert(1,1);
  m = m.insert(2,2);
  m = m.insert(3,3);
  cout << "Initial map 1\n"; m.dump();

  DDL::Map<int,int> m2;
  m2 = m2.insert(1,2);
  m2 = m2.insert(2,2);
  m2 = m2.insert(3,3);
  cout << "Initial map 2\n"; m2.dump();

  cout << "compare: " << DDL::compare(m2,m) << endl;

  cout << "AFTER:\n";
  m.dump();
  m2.dump();
*/
}


int main() {
  auto a = DDL::Array<DDL::UInt<64>>::rangeUp(0,10,2);
  std::cout << a << std::endl;

  auto b = DDL::Array<DDL::UInt<64>>::rangeDown(10,0,1);
  std::cout << b << std::endl;
  return 0;
}
