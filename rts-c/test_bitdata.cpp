#include <iostream>
#include <ddl/number.h>
#include <ddl/size.h>

  namespace User {
    class T_A : public DDL::Bitdata<16> {
    public:
      T_A() : DDL::Bitdata<16>() {}
      T_A(DDL::Bitdata<16> x) : DDL::Bitdata<16>(x) {}

      static
      User::T_A fromBits(DDL::UInt<16> x) {
        return DDL::Bitdata<16>{x};
      }

      T_A init(DDL::UInt<8> v) {
        return fromBits(DDL::UInt<16>{DDL::UInt<8>{1}, v.toBits()});
      }
    };
  }


struct A : public Bitdata<32> {
  A(UInt<bitWidth> x) : Bitdata(x) {}
  A(Bitdata<bitWidth> x) : Bitdata(x) {}
  static A fromBits(UInt<bitWidth> x) { return Bitdata<32>(x); }
};

void test_bitdata() {
  User::T_A v;
  v.init(DDL::UInt<8>(7));


  std::cout << "--- Bitdata -----\n";
  A x(2);
  A y(1);
  Float z(7);
  std::cout << (x == y) << std::endl;
  std::cout << compare(x,y) << std::endl;
  std::cout << bitdata_to_uint(x) << std::endl;
  std::cout << std::hex << bitdata_to_uint(z) << std::endl;
  std::cout << "-----------------\n";
}
