#pragma once
#include <string>
#include <exception>
#include <ddl/array.h>
#include "main_parser.h"
#include "Owned.hpp"

struct EncryptionException : public std::exception {

};

bool removePadding(std::string &str);

struct EncryptionContext {
    std::string key;
    Owned<User::ChooseCiph> cipher;
    EncryptionContext(std::string key, User::ChooseCiph cipher)
    : key(key), cipher(borrowed(cipher)) {}
};

std::string makeObjKey(
    EncryptionContext const& encCtx,
    uint64_t objId,
    uint16_t gen,
    bool isAES
);

std::string makeFileKeyAlg2(
    DDL::Array<DDL::UInt<8>> encO,
    DDL::Integer encP,
    DDL::Array<DDL::UInt<8>> id0,
    std::string password
);

EncryptionContext makeEncryptionContext(User::EncryptionDict dict);
