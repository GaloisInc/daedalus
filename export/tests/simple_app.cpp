
#include <vector>
#include <optional>
#include <iostream>
#include <utility>
#include <cstdint>
#include <cmath>
#include "json.hpp"

#include "parser/main_parser.h"
#include <ddl/parser.h>
#include <ddl/number.h>
#include <ddl/maybe.h>
#include <ddl/array.h>
#include <ddl/export.h>

using nlohmann::basic_json;

uint64_t exp10(uint32_t exp) {
  uint64_t res = 1;
  while (exp != 0) { res *= 10; --exp; }
  return res;
}

unsigned char exportChar(DDL::UInt<8> x) { return x.rep(); }

int64_t exportS64(DDL::SInt<64> x) { return x.rep(); }
int32_t exportS32(DDL::SInt<32> x) { return x.rep(); }

bool exportBool(DDL::Bool x) { return x.getValue(); }

std::string exportString(DDL::Array<DDL::UInt<8>> x) {
  return x.export_array(StringBuilder{}, exportChar);
}

basic_json<> exportNumber(User::JSON_number x) {
  auto res = [=] () mutable {
    auto w = exportS64(x.get_whole());
    auto e = exportS32(x.get_exp());
    if (e < 0) return basic_json<>(w * std::pow(10,e));
    auto pos_e = exp10(static_cast<uint32_t>(e));
    if (w < 0) return basic_json<>(w * static_cast<int64_t>(pos_e));
    return basic_json<>(static_cast<uint64_t>(w) * pos_e);
  }();
  x.free();
  return res;
}

basic_json<> exportJSON(User::JSON_value x) {
  std::vector<User::JSON_value> todo = {x};
  std::vector<std::pair<User::JSON_value,size_t>> in_progress;
  std::vector<basic_json<>> done;

  while (!todo.empty()) {
    User::JSON_value el = todo.back();
    todo.pop_back();
    switch(el.getTag()) {

      case DDL::Tag::JSON_value::Null: {
        done.push_back(basic_json(nullptr));
        el.free();
        break;
      }

      case DDL::Tag::JSON_value::Bool: {
        done.push_back(basic_json(exportBool(el.get_Bool())));
        el.free();
        break;
      }

      case DDL::Tag::JSON_value::Number: {
        done.push_back(exportNumber(el.get_Number()));
        el.free();
        break;
      }
      
      case DDL::Tag::JSON_value::String: {
        done.push_back(basic_json(exportString(el.get_String())));
        el.free();
        break;
      }

      case DDL::Tag::JSON_value::Array: {
        auto arr = el.borrow_Array();
        size_t n = arr.size().rep();
        in_progress.push_back({el,n});
        for (size_t i = 0; i < n; ++i) {
          todo.push_back(arr[i]);
        }
        continue;
      }
      case DDL::Tag::JSON_value::Object: {
        // XXX;

        break; // continue
      }
    }

    while(!in_progress.empty()) {
      auto pair = in_progress.back();
      size_t n = pair.second;
      if (done.size() < n) break;
      auto val = pair.first;
      switch (pair.first.getTag()) {
        case DDL::Tag::JSON_value::Array: {
          val.free();
          std::vector<basic_json<>> result;
          result.reserve(n);
          for (size_t i = 0; i < n; ++i) {
            result.push_back(std::move(done.back()));
            done.pop_back();
          }
          done.push_back(basic_json(std::move(result)));
          continue;
        }
        default:
          std::cerr << "BAD TAG: " << (int)pair.first.getTag() << "\n";
          assert(false);
      }
    }
  }
  assert(done.size() == 1);
  return std::move(done.back());
}


int main() {
  DDL::ParseError<DDL::Input> error;
  std::vector<User::JSON_value> results;
  parseJSON_value_strict(error, results, DDL::Input("(test)", "[1,2,3]"));
  for(auto i : results) {
    auto x = exportJSON(i);
    std::cout << x << "\n";
  }
  if (results.size() == 0) {
    std::cout << "parse error\n";
  }
  
  return 0;
}
