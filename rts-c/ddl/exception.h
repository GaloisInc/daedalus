#ifndef DDL_EXCEPTION_H
#define DDL_EXCEPTION_H

namespace DDL {

// The outcome of calling a non-capturing parser.
enum class ParserResult { Ok, Failure, Exception };

// An exception value thrown by a pure function or parser.
class Exception {
  char const *location;
  char const *message;
public:
  Exception() : location(""), message("") {}
  Exception(char const *loc, char const *msg) : location(loc), message(msg) {}

  char const *getLocation() const { return location; }
  char const *getMessage()  const { return message; }
};

// The result of a pure function that may throw an exception.
// `T` is the normal return type.
template <typename T>
class Result {
  bool is_ok;
  union {
    T         value;
    Exception exception;
  };

public:
  Result() : is_ok(false), exception() {}

  static Result ok(T v) {
    Result r;
    r.is_ok = true;
    r.value = v;
    return r;
  }

  static Result failure(Exception e) {
    Result r;
    r.is_ok = false;
    r.exception = e;
    return r;
  }

  static Result failure(char const *loc, char const *msg) {
    return failure(Exception(loc, msg));
  }

  bool      isOk()           const { return is_ok; }
  T         getValue()       const { return value; }
  Exception getException()   const { return exception; }
};

}

#endif
