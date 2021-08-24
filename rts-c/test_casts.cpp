
#include <cstdint>

#include <ddl/cast.h>
#include <ddl/integer.h>
#include <ddl/number.h>


using namespace std;
using namespace DDL;

void test_casts () {
  UInt<7> u{62};
  UInt<7> u1(1);
  SInt<7> s(-62);
  SInt<7> s1(-1);
  SInt<7> sp(62);
  Integer i(73);
  Integer in(-73);

  cout << "uint_uint 7 16: " << u  << " -> "  << uint_to_uint<7,16>(u) << endl;
  cout << "uint_uint 7 4: "  << u  << " -> "  << uint_to_uint<7,4>(u)  << endl;
  cout << "sint_sint: 7 16"  << s  << " -> "  << sint_to_sint<7,16>(s) << endl;
  cout << "sint_uint: 7 16"  << s1 << " -> " << sint_to_uint<7,16>(s1) << endl;
  cout << "sint_uint: 7 16"  << sp << " -> " << sint_to_uint<7,16>(sp) << endl;
  cout << "uint_sint: 7 16"  << u  << " -> " << uint_to_sint<7,16>(u) << endl;
  cout << "uint_integer: 7"  << u  << " -> " << uint_to_integer<7>(u) << endl;
  cout << "sint_integer: 7"  << s  << " -> " << sint_to_integer<7>(s) << endl;
  cout << "sint_integer: 7"  << sp << " -> " << sint_to_integer<7>(sp) << endl;

  cout << "uint_to_uint_maybe: 7 6" << u <<
                          " -> " << uint_to_uint_maybe<7,6>(u) << endl;
  cout << "uint_to_uint_maybe: 7 6" << u1 <<
                          " -> " << uint_to_uint_maybe<7,6>(u1) << endl;

  cout << "sint_to_sint_maybe: 7 6" << s <<
                          " -> " << sint_to_sint_maybe<7,6>(s) << endl;
  cout << "sint_to_sint_maybe: 7 6" << s1 <<
                          " -> " << sint_to_sint_maybe<7,6>(s1) << endl;

  cout << "uint_to_sint_maybe: 7 6" << u <<
                          " -> " << uint_to_sint_maybe<7,6>(u) << endl;

  cout << "uint_to_sint_maybe: 7 7" << u <<
                          " -> " << uint_to_sint_maybe<7,7>(u) << endl;

  cout << "uint_to_sint_maybe: 7 8" << u <<
                          " -> " << uint_to_sint_maybe<7,8>(u) << endl;

  cout << "uint_to_sint_maybe: 7 6" << u1 <<
                          " -> " << uint_to_sint_maybe<7,6>(u1) << endl;

  cout << "sint_to_uint_maybe: 7 6" << s <<
                          " -> " << sint_to_uint_maybe<7,6>(s) << endl;

  cout << "sint_to_uint_maybe: 7 6" << sp <<
                          " -> " << sint_to_uint_maybe<7,6>(sp) << endl;

  cout << "sint_to_uint_maybe: 7 7" << sp <<
                          " -> " << sint_to_uint_maybe<7,7>(sp) << endl;

  cout << "integer_to_uint_maybe: 7" << i <<
                          " -> " << integer_to_uint_maybe<7>(i) << endl;

  cout << "integer_to_uint_maybe: 6" << i <<
                          " -> " << integer_to_uint_maybe<6>(i) << endl;

  cout << "integer_to_uint_maybe: 7" << in <<
                          " -> " << integer_to_uint_maybe<7>(in) << endl;

  cout << "integer_to_sint_maybe: 7" << i <<
                          " -> " << integer_to_sint_maybe<7>(i) << endl;

  cout << "integer_to_sint_maybe: 8" << i <<
                          " -> " << integer_to_sint_maybe<8>(i) << endl;

  cout << "integer_to_sint_maybe: 8" << in <<
                          " -> " << integer_to_sint_maybe<8>(in) << endl;


}


