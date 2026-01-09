#include "parser/main_parser.h"
#include <ddl/parser.h>
#include <ddl/number.h>
#include <ddl/maybe.h>
#include <vector>
#include <optional>
#include <iostream>

struct S {
  int x;
  int y;
};


template<unsigned N>
int export_uint_to_int(DDL::UInt<N> x) {
  return int{x.rep()};
}



S exportMain(User::Main x) {
  return {
    .x = export_uint_to_int<8>(x.get_x()),
    .y = export_uint_to_int<16>(x.get_y())
  };
}


std::optional<int> exportMaybe(DDL::Maybe<DDL::UInt<8>> m) {
  if (m.isNothing()) {
    return std::optional<int>{};
  } else {
    return std::optional<int>{export_uint_to_int<8>(m.getValue())};
  }
}

template<typename A, typename B, typename F>
std::optional<B> exportMaybePoly(F f, DDL::Maybe<A> m) {
  if (m.isNothing()) {
    return std::optional<B>{};
  } else {
    return std::optional<B>{f(m.getValue())};
  }
}

std::optional<int> exportMaybeV2(DDL::Maybe<DDL::UInt<8>> m) {
  return exportMaybePoly<DDL::UInt<8>, int>(export_uint_to_int<8>, m);
}


int main() {
  DDL::ParseError<DDL::Input> error;
  std::vector<User::Main> results;
  parseMain(error, results, DDL::Input());
  for(auto i : results) {
    auto s = exportMain(i);
    std::cout << "x = " << s.x << "; y = " << s.y << "\n";
  }
  if (results.size() == 0) {
    std::cout << "parse error\n";
  }
  
  return 0;
}