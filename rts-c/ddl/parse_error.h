#ifndef DDL_PARSE_ERROR_H
#define DDL_PARSE_ERROR_H

#include <cstddef>
#include <cstdint>
#include <deque>
#include <iostream>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <ddl/debug.h>
#include <ddl/owned.h>
#include <ddl/boxed.h>
#include <ddl/json.h>
#include <ddl/number.h>
#include <ddl/array.h>

namespace DDL {

struct ParserContextCall {
  char const *label;
  size_t count;
};

struct ParserContextOmitted {
  size_t count;
};

using ParserContextEntry =
  std::variant<ParserContextCall,ParserContextOmitted>;

class ParserContextFrame {
  static constexpr size_t maxTailCallRuns = 64;

  struct Call {
    char const *label;
  };

  struct TailCallRun {
    char const *label;
    size_t count;
  };

  struct TailCalls {
    char const *entry;
    std::deque<TailCallRun> recent;
    size_t omitted;
  };

  std::variant<Call,TailCalls> frame;

  static bool sameCallSite(char const *x, char const *y) {
    return std::string_view(x) == std::string_view(y);
  }

public:
  explicit ParserContextFrame(char const* label) : frame(Call { label }) {}

  void tailCall(char const* label) {
    if (auto *call = std::get_if<Call>(&frame)) {
      auto entry = call->label;
      std::deque<TailCallRun> recent;
      recent.push_back(TailCallRun { label, 1 });
      frame = TailCalls { entry, std::move(recent), 0 };
      return;
    }

    auto &calls = std::get<TailCalls>(frame);
    if (!calls.recent.empty() &&
        sameCallSite(calls.recent.back().label,label)) {
      calls.recent.back().count++;
      return;
    }

    if (calls.recent.size() == maxTailCallRuns) {
      calls.omitted += calls.recent.front().count;
      calls.recent.pop_front();
    }
    calls.recent.push_back(TailCallRun { label, 1 });
  }

  bool isCall() const {
    return std::holds_alternative<Call>(frame);
  }

  char const* callSiteLabel() const {
    return std::get<Call>(frame).label;
  }

  std::vector<ParserContextEntry> entries() const {
    if (auto const *call = std::get_if<Call>(&frame)) {
      return { ParserContextCall { call->label, 1 } };
    }

    auto const &calls = std::get<TailCalls>(frame);
    std::vector<ParserContextEntry> result;
    size_t firstRecent = 0;

    if (calls.omitted == 0 &&
        sameCallSite(calls.entry,calls.recent.front().label)) {
      result.push_back(
        ParserContextCall {
          calls.entry,
          calls.recent.front().count + 1
        }
      );
      firstRecent = 1;
    } else {
      result.push_back(ParserContextCall { calls.entry, 1 });
    }

    if (calls.omitted > 0) {
      result.push_back(ParserContextOmitted { calls.omitted });
    }

    for (size_t i = firstRecent; i < calls.recent.size(); ++i) {
      auto const &run = calls.recent[i];
      result.push_back(ParserContextCall { run.label, run.count });
    }
    return result;
  }
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

    if (frame.isCall()) {
      os << JS(std::string_view(frame.callSiteLabel()));
      continue;
    }

    os << "[ ";
    bool firstEntry = true;
    auto separator = [&]() {
      if (!firstEntry) os << "\n, ";
      firstEntry = false;
    };
    for (auto const &entry : frame.entries()) {
      separator();
      if (auto const *call = std::get_if<ParserContextCall>(&entry)) {
        auto label = call->label;
        auto count = call->count;
        if (count > 1) {
          os << "[" << JS(std::string_view(label))
             << ", " << count << "]";
        } else {
          os << JS(std::string_view(label));
        }
      } else {
        auto omitted = std::get<ParserContextOmitted>(entry).count;
        os << "{ \"omitted\": " << omitted << " }";
      }
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
    os << "    •";
    for (auto const &entry : frame.entries()) {
      if (auto const *call = std::get_if<ParserContextCall>(&entry)) {
        auto label = call->label;
        auto count = call->count;
        os << " " << label;
        if (count > 1) {
          os << " (" << count << " times)";
        }
      } else {
        auto omitted = std::get<ParserContextOmitted>(entry).count;
        os << " ... (" << omitted << " tail calls omitted)";
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
