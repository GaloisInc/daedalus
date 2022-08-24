#pragma once

#include <string>
#include <exception>

#include <openssl/evp.h>

#include <ddl/array.h>
#include <ddl/owned.h>

#include <pdfcos.hpp>

struct EncryptionException : public std::exception {

};

bool removePadding(std::string &str);

struct EncryptionContext {
    std::vector<uint8_t> key;
    DDL::Owned<PdfCos::ChooseCiph> cipher;
    EncryptionContext(std::vector<uint8_t> key, PdfCos::ChooseCiph cipher)
    : key(key), cipher(borrowed(cipher)) {}
};

std::vector<uint8_t> makeObjKey(
    EncryptionContext const& encCtx,
    uint64_t objId,
    uint16_t gen,
    bool isAES
);

std::vector<uint8_t> makeFileKeyAlg2(
    DDL::Array<DDL::UInt<8>> encO,
    DDL::Integer encP,
    DDL::Array<DDL::UInt<8>> id0,
    std::vector<uint8_t> const& password
);

EncryptionContext makeEncryptionContext(PdfCos::EncryptionDict dict);

bool aes_cbc_decryption(
    EVP_CIPHER const* alg,
    char const* input, size_t input_len,
    char const* key,
    std::string &output);
