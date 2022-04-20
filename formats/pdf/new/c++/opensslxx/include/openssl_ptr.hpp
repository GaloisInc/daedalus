#pragma once

#include <openssl/evp.h>

#include <memory>

namespace opensslxx {
    template<class T> struct DeleterOf;
    template<> struct DeleterOf<EVP_CIPHER_CTX> { void operator()(EVP_CIPHER_CTX *p) const { EVP_CIPHER_CTX_free(p); } };
    template<> struct DeleterOf<EVP_MD_CTX> { void operator()(EVP_MD_CTX *p) const { EVP_MD_CTX_free(p); } };

    template<class OpenSSLType>
    using OpenSSL_ptr = std::unique_ptr<OpenSSLType, DeleterOf<OpenSSLType>>;
}