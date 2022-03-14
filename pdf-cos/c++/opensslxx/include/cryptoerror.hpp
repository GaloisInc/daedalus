#pragma once

#include <exception>

struct OpenSSLXX_exception : public std::exception {
    unsigned long code;
    OpenSSLXX_exception(unsigned long code);
    char const* what() const throw() override;
};

[[noreturn]] void throw_openssl_error();