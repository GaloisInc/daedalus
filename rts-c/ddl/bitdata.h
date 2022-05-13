#ifndef DDL_bitdata_H
#define DDL_bitdata_H

#include <ddl/number.h>
#include <ddl/value.h>
#include <ddl/size.h>

namespace DDL  {

// This class is used to share common code for user-defined bitdata types.
template<Width n>
class Bitdata : public Value {
  UInt<n> data;
public:
  static constexpr Width bitWidth = n;

  Bitdata()          : data(UInt<n>()) {}
  Bitdata(UInt<n> d) : data(d) {}

  UInt<n> toBits() { return data; }

  bool operator == (Bitdata x) { return data == x.toBits(); }
  bool operator != (Bitdata x) { return data != x.toBits(); }
  bool operator <= (Bitdata x) { return data <= x.toBits(); }
  bool operator >= (Bitdata x) { return data >= x.toBits(); }
  bool operator <  (Bitdata x) { return data <  x.toBits(); }
  bool operator >  (Bitdata x) { return data >  x.toBits(); }
};

template<Width n>
int compare(Bitdata<n> x, Bitdata<n> y) {
  return compare(x.toBits(),y.toBits());
}

}
#endif
