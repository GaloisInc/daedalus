#pragma once

#include <openssl/evp.h>

#include <vector>
#include <cstdint>
#include "openssl_ptr.hpp"

namespace opensslxx {

enum class CipherDirection {
    decrypt = 0, encrypt = 1
};

class Cipher {
    OpenSSL_ptr<EVP_CIPHER_CTX> ctx;
public:
    Cipher(EVP_CIPHER_CTX *ctx);

    void init(EVP_CIPHER const* cipher, unsigned char const* key, unsigned char const* iv, CipherDirection dir);
    int update(unsigned char *out, int outlen, unsigned char const* in, int inlen);
    int final(unsigned char *out, int outlen);
};

Cipher make_cipher();

}