#pragma once

#include <string>
#include <exception>

#include "bitstream.hpp"

struct LzwException : public std::exception {
    char const* msg;
    LzwException(char const*);
    LzwException() = delete;
    const char * what () const throw () override;
};

std::string decompress(BitStream bits);
