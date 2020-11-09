#include<ddl/cast.h>

using namespace std;
using namespace DDL;

void cast_tests() {
  UInt<7> u(72);
  UInt<7> u1(1);
  SInt<7> s(-72);
  SInt<7> s1(-1);
  SInt<7> sp(72);
  Integer i(73L);
  Integer in(-73L);

  cout << "uint_uint: "    << u  << " -> "  << uint_to_uint<7,16>(u) << endl;
  cout << "uint_uint: "    << u  << " -> "  << uint_to_uint<7,4>(u)  << endl;
  cout << "sint_sint: "    << s  << " -> "  << sint_to_sint<7,16>(s) << endl;
  cout << "sint_uint: "    << s1 << " -> " << sint_to_uint<7,16>(s1) << endl;
  cout << "sint_uint: "    << sp << " -> " << sint_to_uint<7,16>(sp) << endl;
  cout << "uint_sint: "    << u  << " -> " << uint_to_sint<7,16>(u) << endl;
  cout << "uint_integer: " << u  << " -> " << uint_to_integer<7>(u) << endl;
  cout << "sint_integer: " << s  << " -> " << sint_to_integer<7>(s) << endl;
  cout << "sint_integer: " << sp << " -> " << sint_to_integer<7>(sp) << endl;

  cout << "uint_to_uint_maybe: " << u <<
                          " -> " << uint_to_uint_maybe<7,6>(u) << endl;
  cout << "uint_to_uint_maybe: " << u1 <<
                          " -> " << uint_to_uint_maybe<7,6>(u1) << endl;

  cout << "sint_to_sint_maybe: " << s <<
                          " -> " << sint_to_sint_maybe<7,6>(s) << endl;
  cout << "sint_to_sint_maybe: " << s1 <<
                          " -> " << sint_to_sint_maybe<7,6>(s1) << endl;

  cout << "uint_to_sint_maybe: " << u <<
                          " -> " << uint_to_sint_maybe<7,6>(u) << endl;

  cout << "uint_to_sint_maybe: " << u <<
                          " -> " << uint_to_sint_maybe<7,7>(u) << endl;

  cout << "uint_to_sint_maybe: " << u <<
                          " -> " << uint_to_sint_maybe<7,8>(u) << endl;

  cout << "uint_to_sint_maybe: " << u1 <<
                          " -> " << uint_to_sint_maybe<7,6>(u1) << endl;

  cout << "sint_to_uint_maybe: " << s <<
                          " -> " << sint_to_uint_maybe<7,6>(s) << endl;

  cout << "sint_to_uint_maybe: " << sp <<
                          " -> " << sint_to_uint_maybe<7,6>(sp) << endl;

  cout << "sint_to_uint_maybe: " << sp <<
                          " -> " << sint_to_uint_maybe<7,7>(sp) << endl;

  cout << "integer_to_uint_maybe: " << i <<
                          " -> " << integer_to_uint_maybe<7>(i) << endl;

  cout << "integer_to_uint_maybe: " << i <<
                          " -> " << integer_to_uint_maybe<6>(i) << endl;

  cout << "integer_to_uint_maybe: " << in <<
                          " -> " << integer_to_uint_maybe<7>(in) << endl;

  cout << "integer_to_sint_maybe: " << i <<
                          " -> " << integer_to_sint_maybe<7>(i) << endl;

  cout << "integer_to_sint_maybe: " << i <<
                          " -> " << integer_to_sint_maybe<8>(i) << endl;

  cout << "integer_to_sint_maybe: " << in <<
                          " -> " << integer_to_sint_maybe<8>(in) << endl;



}


