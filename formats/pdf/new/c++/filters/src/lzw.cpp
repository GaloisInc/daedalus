#include "lzw.hpp"
#include "bitstream.hpp"

#include <vector>
#include <iostream>
#include <utility>
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
    if (code != 256) {
        throw LzwException("LZW stream does not start with a clear-table code");
    }

    std::string prev;
    std::string result;
    bool have_prev = false;

    for(;;) {
        code = bits.get(codelen);
        if (code == -1) {
            throw LzwException("Insufficient bits");
        } else if (code == 256) {
            codelen = 9;
            dictionary.resize(258);
            prev.clear();
            have_prev = false;
            continue; // avoid adding an extry to the table
        } else if (code == 257) {
            return result;
        }

        if (!have_prev) {
            if (code >= 256) {
                throw LzwException("Invalid first code after clear-table code");
            }
            prev = dictionary[code];
            result += prev;
            have_prev = true;
            continue;
        }

        std::string entry;
        if (code < dictionary.size()) {
            entry = dictionary[code];
        } else if (code == dictionary.size()) {
            entry = prev + prev[0];
        } else {
            std::cerr << "Got code " << code << " but dictionary has " << dictionary.size() << std::endl;
            throw LzwException("Code out of range");
        }

        result += entry;
        dictionary.push_back(prev + entry[0]);
        prev = std::move(entry);
  
        // PDF defines the maximum dictionary size to be 4096
        // and the maximum code to be 12-bits
        switch (dictionary.size()) {
            case 4097: throw LzwException("Table was full"); // clear table code expected
            case 512: case 1024: case 2048: codelen++; /* fall through */
        }
    }
}
