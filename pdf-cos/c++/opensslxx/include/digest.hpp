#pragma once

#include <openssl/evp.h>
#include <vector>
#include <cstdint>
#include "openssl_ptr.hpp"

namespace opensslxx {

class Digest {
    OpenSSL_ptr<EVP_MD_CTX> ctx;

public:
    Digest(EVP_MD_CTX *ctx);

    void init(EVP_MD const* md);
    void update(void const* d, size_t cnt);
    std::vector<uint8_t> final();

    friend Digest make_digest();
};

Digest make_digest();

std::vector<uint8_t> digest(EVP_MD const* mdtype, void const* input, size_t len);

}
