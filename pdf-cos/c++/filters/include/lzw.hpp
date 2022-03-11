#pragma once

#include <exception>
#include <cstdint>
#include <string>

struct LzwException : public std::exception {
    char const* msg;
    LzwException(char const*);
    LzwException() = delete;
    const char * what () const throw () override;
};

std::string decompress(uint8_t const* bits, size_t len);
