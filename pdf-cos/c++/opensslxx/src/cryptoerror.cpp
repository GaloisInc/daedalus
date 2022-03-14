#include "cryptoerror.hpp"

#include <openssl/err.h>

OpenSSLXX_exception::OpenSSLXX_exception(unsigned long code) : code(code) {}

char const* OpenSSLXX_exception::what() const throw() {
    return "OpenSSL error";
}

[[noreturn]] void throw_openssl_error()
{
    unsigned long code = ERR_get_error();
    while (0 != ERR_get_error());
    throw OpenSSLXX_exception(code);
}