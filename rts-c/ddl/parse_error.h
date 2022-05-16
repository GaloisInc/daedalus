#ifndef DDL_PARSE_ERROR_H
#define DDL_PARSE_ERROR_H

#include <cstdint>
#include <iostream>
#include <vector>
#include <unordered_map>

#include <ddl/debug.h>
#include <ddl/input.h>
#include <ddl/stack.h>

namespace DDL {

class ParserContextFrame {
  char const *cur;
  std::unordered_map<std::string_view, size_t> history;

public:
  explicit ParserContextFrame(char const* fun) : cur(fun) {}

  void tailCall(char const* fun) {
      auto [thing, inserted] = history.insert({cur,1});
      if (!inserted) thing->second++;
      cur = fun;
  }

  char const* get_cur() const { return cur; }

  auto const& get_history() const { return history; }
};

class ParserContextStack {
  std::vector<ParserContextFrame> stack;
public:
  ParserContextStack() = default;

  void callFun(char const* fun) {
    stack.emplace_back(fun);
  }

  void tailCallFun(char const* fun) {
    if (stack.empty()) callFun(fun);
    else stack.back().tailCall(fun);
  }
  void popFun() { if (!stack.empty()) stack.pop_back(); }

  auto begin() { return stack.begin(); }
  auto end() { return stack.end(); }
};


struct ParseError {
  Size offset;
  ParserContextStack debugs;
};



}
#endif
