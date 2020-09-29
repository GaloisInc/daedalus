#ifndef DDL_INTEGER
#define DDL_INTEGER

#include <memory>
#include <gmpxx.h>

namespace DDL {



struct Integer {
  std::shared_ptr<mpz_class> data;
public:
  Integer(const char* str) : data(std::make_shared<mpz_class>(str)) {}
  Integer(std::shared_ptr<mpz_class> x) : data(x) {}

  template <int op>

  Integer operator + (Integer x) {
    if      (data.use_count() == 1) { *data = *data + *x.data; return *this; }
    else if (x.data.use_count() == 1) { *x.data = *data + *x.data; return x; }
    else return std::make_shared<mpz_class>(*data + *x.data);
  }

  Integer operator - (Integer x) {
    if      (data.use_count() == 1) { *data = *data - *x.data; return *this; }
    else if (x.data.use_count() == 1) { *x.data = *data - *x.data; return x; }
    else return std::make_shared<mpz_class>(*data - *x.data);
  }

  Integer operator * (Integer x) {
    if      (data.use_count() == 1) { *data = *data * *x.data; return *this; }
    else if (x.data.use_count() == 1) { *x.data = *data * *x.data; return x; }
    else return std::make_shared<mpz_class>(*data * *x.data);
  }

  // XXX: check for division by 0?
  Integer operator / (Integer x) {
    if      (data.use_count() == 1) { *data = *data / *x.data; return *this; }
    else if (x.data.use_count() == 1) { *x.data = *data / *x.data; return x; }
    else return std::make_shared<mpz_class>(*data / *x.data);
  }

  Integer operator - () {
    if (data.use_count() == 1) { *data = - *data; return *this; }
    else return std::make_shared<mpz_class>(- *data);
  }

  // Assumes `x.data` is not NULL
  friend
  std::ostream& operator<<(std::ostream& os, Integer x) {
    os << *x.data;
    return os;
  }

  friend std::hash<Integer>;
};

}

namespace std {
  template<>
  struct hash<DDL::Integer> {
    std::size_t operator()(DDL::Integer x) const noexcept {
      return hash<mpz_class>{}(*x.data);
    }
  };
}


#endif


