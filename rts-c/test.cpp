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
#include "ddl/map.h"

#include "user_data.h"
#include "fun_call.h"
#include "test.h"

using namespace std;


void iterator_test() {
  DDL::Map<int,int> m;
  m = m.insert(1,1);
  m = m.insert(2,2);
  m = m.insert(3,3);
  cout << "Initial map\n"; m.dump();

  m.copy();
  cout << "Map copied map\n"; m.dump();

  DDL::Map<int,int>::Iterator it(m);
  cout << "Created iterator\n"; m.dump();

  it.copy();
  cout << "Copied iterator\n"; m.dump();
  DDL::Map<int,int>::Iterator jt = it;

  cout << "Using 1st iterator\n";
  while (!it.done()) {
    cout << it.borrowKey() << " = " << it.borrowValue() << endl;
    it = it.next();
    m.dump();
  }

  cout << "Using 2nd iterator\n";
  while (!jt.done()) {
    cout << jt.borrowKey() << " = " << jt.borrowValue() << endl;
    jt = jt.next();
    m.dump();
  }


}


int main() {

  // example();
  // cast_tests();

  iterator_test();

  // rb_tests();

  return 0;
}
