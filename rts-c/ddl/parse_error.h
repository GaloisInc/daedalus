#ifndef DDL_PARSE_ERROR_H
#define DDL_PARSE_ERROR_H

#include <cstdint>
#include <iostream>
#include <vector>
#include <unordered_map>

#include <ddl/debug.h>
#include <ddl/owned.h>
#include <ddl/boxed.h>
#include <ddl/json.h>
#include <ddl/number.h>
#include <ddl/array.h>

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

  auto begin() const { return stack.begin(); }
  auto end() const { return stack.end(); }

  auto cbegin() { return stack.cbegin(); }
  auto cend() { return stack.cend(); }
};


template <typename I>
struct ParseError {
  Owned<I> input;
  Owned<Array<UInt<8>>> message;
  bool is_system_error;
  ParserContextStack debugs;
  char const *error_loc;

  ParseError() : input(Owned(I()))
               , message(Owned(Array<UInt<8>>()))
               , is_system_error(true)
               , error_loc("")
                 {}

  // input messgae
  // Owns array, message
  explicit ParseError( bool is_sys
                     , char const *loc
                     , I input
                     , Array<UInt<8>> message
                     , ParserContextStack const& debugs
                     ) : input(input)
                       , message(message)
                       , is_system_error(is_sys)
                       , debugs(debugs)
                       , error_loc(loc)
                         {}


  // Add another error to the set.
  // Borrows newInput, newMsg
  void improve( bool newIsSys
              , char const *loc
              , I newInput
              , Array<UInt<8>> newMsg
              , ParserContextStack const& newDebugs
              ) {

    // user messages takes precedence over system messages
    if (newIsSys && !is_system_error) return;

    // if they are the same type, then we check offsets
    // XXX: comparing offsets only really makes sense for the same input.
    if (newIsSys == is_system_error) {
      Size offset    = input->getOffset();
      Size newOffset = newInput.getOffset();
      if (newOffset < offset) return;
    }

    // We found a better error.
    is_system_error = newIsSys;
    error_loc = loc;
    input.assignBorrowed(newInput);
    message.assignBorrowed(newMsg);
    debugs  = newDebugs;
  }
};

template <typename I>
static inline
std::ostream& toJS(std::ostream &os, ParseError<I> const& err) {
  auto const &inp = err.input.borrow();

  os << "{ \"error\": " << JS(err.message.borrow().borrowBytes());
  os << "\n, \"offset\": " << inp.getOffset();
  os << "\n, \"context\":\n[";
  bool first = true;
  for (auto&& frame : err.debugs) {
    if (!first) os << "\n, ";
    first = false;

    auto cur = frame.get_cur();
    auto const& h = frame.get_history();
    auto in_hist = h.find(cur);

    os << "[ ";

    size_t n = 1;
    if (in_hist != h.end()) {
      n += in_hist->second;
    }
    if (n > 1)
      os << "[" << JS(std::string_view(cur)) << ", " << n << "]";
    else
      os << JS(std::string_view(cur));

    for (auto &&el : h) {
      if (el.first == cur) continue;
      os << "\n, ";
      if (el.second > 1)
        os << "[" << JS(el.first) << ", " << el.second << "]";
      else
        os << JS(el.first);
    }
    os << "]";
  }
  os << "]";

  if (err.error_loc != nullptr && *err.error_loc != 0) {
    os << "\n, \"location\": " << JS(std::string_view(err.error_loc));
  }

  return os << "}";
}

template <typename I>
static inline
std::ostream& operator << (std::ostream &os, ParseError<I> const& err) {
  auto const &inp = err.input.borrow();

  // assumes a simple encoding for the name.
  os << inp.borrowNameBytes();
  os << ":[offset " << inp.getOffset() << "]";
  os << std::endl;
  os << "  • " << err.message.borrow().borrowBytes();
  os << std::endl;
  os << "  • Grammar context:";
  os << std::endl;
  for (auto&& frame : err.debugs) {
    os << "    • ";

    auto cur = frame.get_cur();
    os << cur;
    auto const& h = frame.get_history();
    auto in_hist = h.find(cur);

    if (in_hist != h.end()) {
      auto n = in_hist->second;
      if (n > 1) os << " (" << (n+1) << " times)";
    }

    for (auto &&el : frame.get_history()) {
      if (el.first == cur) continue;
      os << " " << el.first;
      if (el.second > 1) {
        os << " (" << el.second << " times)";
      }
    }
    os << std::endl;
  }

  if (err.error_loc != nullptr && *err.error_loc != 0) {
    os << "    • " << err.error_loc << std::endl;
  }

  return os;
}




}
#endif
