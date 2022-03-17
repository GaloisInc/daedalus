#include "cipher.hpp"
#include "cryptoerror.hpp"

namespace opensslxx {

Cipher::Cipher(EVP_CIPHER_CTX * ctx) : ctx(ctx) {}

void Cipher::init(EVP_CIPHER const* cipher, unsigned char const* key, unsigned char const* iv, CipherDirection encrypt) {
    if (!EVP_CipherInit(ctx.get(), cipher, key, iv, int(encrypt)))
        throw_openssl_error();
}

int Cipher::update(unsigned char *out, int outlen, unsigned char const* in, int inlen) {
    if (!EVP_CipherUpdate(ctx.get(), out, &outlen, in, inlen))
        throw_openssl_error();
    return outlen;
}

int Cipher::final(unsigned char *out, int outlen)
{
    if (!EVP_CipherFinal(ctx.get(), out, &outlen))
        throw_openssl_error();
    return outlen;
}

Cipher make_cipher()
{
    auto ctx = EVP_CIPHER_CTX_new();
    if (ctx == NULL) {
        throw_openssl_error();
    }
    return ctx;
}

void Cipher::set_padding(bool padding)
{
    EVP_CIPHER_CTX_set_padding(ctx.get(), padding);
}

}
