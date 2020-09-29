#ifndef DDL_INTEGER
#define DDL_INTEGER

#include <memory>
#include <gmpxx.h>

namespace DDL {

// This ends up with two pointers:
// we have a pointer to an mpt_z, which contains a pointer to the libs.
class Integer {
  std::shared_ptr<mpz_class> data;
public:
  Integer(const char* str) : data(std::make_shared<mpz_class>(str)) {}
  Integer(std::shared_ptr<mpz_class> x) : data(x) {}

  template <int op>

  Integer operator + (Integer x) {
    return std::make_shared<mpz_class>(*data + *x.data);
  }

  Integer operator - (Integer x) {
    return std::make_shared<mpz_class>(*data - *x.data);
  }

  Integer operator * (Integer x) {
    return std::make_shared<mpz_class>(*data * *x.data);
  }

  // XXX: check for division by 0?
  Integer operator / (Integer x) {
    return std::make_shared<mpz_class>(*data / *x.data);
  }

  Integer operator - () {
    return std::make_shared<mpz_class>(- *data);
  }

  bool operator == (Integer x) { return *data == *x.data; }
  bool operator != (Integer x) { return *data != *x.data; }
  bool operator <  (Integer x) { return *data <  *x.data; }
  bool operator <= (Integer x) { return *data <= *x.data; }

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

    size_t operator()(DDL::Integer x) const noexcept {
      size_t result = 17;
      mpz_srcptr info = x.data->get_mpz_t();
      size_t n = abs(info->_mp_size);
      for (size_t i = 0; i < n; ++i)
        result = 23 * result + (size_t)(info->_mp_d[i]);
      return result;
    }
  };
}


#endif


