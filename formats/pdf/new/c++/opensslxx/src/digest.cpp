#include <iostream>
#include <openssl/evp.h>

#include "digest.hpp"

#include "cryptoerror.hpp"

namespace opensslxx {

Digest::Digest(EVP_MD_CTX * ctx) : ctx(ctx) {}

void Digest::init(EVP_MD const* md) {
    if (!EVP_DigestInit(ctx.get(), md)) throw_openssl_error();
}

void Digest::update(void const* d, size_t cnt) {
    if (!EVP_DigestUpdate(ctx.get(), d, cnt)) throw_openssl_error();
}

std::vector<uint8_t> Digest::final()
{
    std::vector<uint8_t>result(size_t(EVP_MAX_MD_SIZE), uint8_t(0));
    unsigned int s = result.size();
    if (!EVP_DigestFinal(ctx.get(), result.data(), &s)) throw_openssl_error();
    result.resize(s);
    return result;
}

Digest make_digest()
{
    auto ctx = EVP_MD_CTX_new();
    if (ctx == NULL) {
        throw_openssl_error();
    }
    return Digest(ctx);
}

std::vector<uint8_t> digest(EVP_MD const* mdtype, void const* input, size_t len) {
    std::vector<uint8_t> result(size_t(EVP_MAX_MD_SIZE), 0);
    unsigned int sz = result.size();
    if (!EVP_Digest(input, len, result.data(), &sz, mdtype, nullptr))
        throw_openssl_error();
    result.resize(sz);
    return result;
}

}
