#include "lzw.hpp"
#include <vector>
#include <iostream>
LzwException::LzwException(char const* msg) : msg(msg) {}

const char * LzwException::what () const throw ()
{
    return msg;
}

std::string decompress(uint8_t const* ptr, size_t len) {

    int codelen = 9;

    BitStream bits {ptr, len};

    std::vector<std::string> dictionary;
    for (int i = 0; i < 256; i++) {
        dictionary.push_back({char(i)});
    }

    dictionary.push_back(""); // clear table
    dictionary.push_back(""); // end of data

    int code = bits.get(codelen);
    if (code == -1) {
        throw LzwException("Insufficient bits at start");
    }

    std::string prev {char(code)};
    std::string result {prev};

    for(;;) {
        code = bits.get(codelen);
        if (code == -1) {
            throw LzwException("Insufficient bits");
        } else if (code == 256) {
            codelen = 9;
            dictionary.resize(258);
            continue; // avoid adding an extry to the table
        } else if (code == 257) {
            return result;
        } else if (code < dictionary.size()) {
            std::string const& entry = dictionary[code];
            result += entry;
            prev = entry;
            dictionary.push_back(prev + prev[0]);
        } else if (code == dictionary.size()) {
            prev += prev[0];
            result += prev;
            dictionary.push_back(prev);
        } else {
            std::cerr << "Got code " << code << " but dictionary has " << dictionary.size() << std::endl;
            throw LzwException("Code out of range");
        }
  
        // PDF defines the maximum dictionary size to be 4096
        // and the maximum code to be 12-bits
        switch (dictionary.size()) {
            case 4097: throw LzwException("Table was full"); // clear table code expected
            case 512: case 1024: case 2048: codelen++; /* fall through */
        }
    }
}
