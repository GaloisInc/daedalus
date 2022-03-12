#include "encryption.hpp"

#include <openssl/evp.h>
#include <ddl/integer.h>
#include "main_parser.h"
#include <vector>

bool removePadding(std::string &str)
{
    if (str.empty())
        return false;

    if (str.size() % 16)
        return false;
    
    char p = str.back();
    if (p < 0 || p > 15)
        return false;

    size_t padlen = p;
    if (padlen == 0)
        padlen = 16;

    for (int i = 0; i < padlen; i++) {
        if (str[str.size() - 1 - i] != p)
            return false;
    }

    str.resize(str.size() - padlen);
    return true;
}

std::string makeObjKey(
    EncryptionContext const& encCtx,
    uint64_t objId,
    uint16_t gen,
    bool isAES
) { 
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();

    EVP_DigestInit(ctx, EVP_md5());
    
    EVP_DigestUpdate(ctx, encCtx.key.data(), encCtx.key.size());
    
    // Specified to use 3 bytes of refid and 2 bytes of gen in little-endian encoding
    uint8_t idbytes[] = {
        uint8_t(objId >> (0*8)),
        uint8_t(objId >> (1*8)),
        uint8_t(objId >> (2*8)),
        uint8_t(gen   >> (0*8)),
        uint8_t(gen   >> (1*8)),
    };
    
    EVP_DigestUpdate(ctx, idbytes, std::size(idbytes));

    if (isAES) {
        const uint8_t salt[] = {0x73, 0x41, 0x6C, 0x54};
        EVP_DigestUpdate(ctx, salt, std::size(salt));
    }

    unsigned char md[EVP_MAX_MD_SIZE] = {0};
    unsigned int size = std::size(md);
    EVP_DigestFinal(ctx, md, &size);

    std::string result{ reinterpret_cast<char const*>(md), size_t(size)};

    EVP_MD_CTX_free(ctx);

    return result;
}

std::string makeFileKeyAlg2(
    DDL::Array<DDL::UInt<8>> encO,
    DDL::Integer encP,
    DDL::Array<DDL::UInt<8>> id0,
    std::string password
){
    const uint8_t padding[] {
        0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41
    , 0x64, 0x00, 0x4E, 0x56, 0xFF, 0xFA, 0x01, 0x08
    , 0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68, 0x3E, 0x80
    , 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A};

    EVP_MD const* md5 = EVP_get_digestbyname("MD5"); 
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    
    EVP_DigestInit(ctx, md5);
    
    EVP_DigestUpdate(ctx, password.data(), std::min(password.size(), size_t(32)));
    
    if (password.size() < 32) {
        EVP_DigestUpdate(ctx, padding, 32 - password.size());
    }

    std::vector<uint8_t> hashInput;
    hashInput.reserve(encO.size().value);
    for (DDL::Size i = 0; i < encO.size(); i.increment()) {
        hashInput.push_back(encO.borrowElement(i).rep());
    }
    EVP_DigestUpdate(ctx, hashInput.data(), hashInput.size());

    uint32_t pval = encP.asSize().value;
    uint8_t pbytes[4] = { uint8_t(pval >> (0*8)), uint8_t(pval >> (1*8)), uint8_t(pval >> (2*8)), uint8_t(pval >> (3*8)) };
    EVP_DigestUpdate(ctx, pbytes, std::size(pbytes));

    hashInput.clear();
    hashInput.reserve(id0.size().value);
    for (DDL::Size i = 0; i < id0.size(); i.increment()) {
        hashInput.push_back(id0.borrowElement(i).rep());
    }
    EVP_DigestUpdate(ctx, hashInput.data(), hashInput.size());

    // XXX: If document metadata is not being encrypted, pass 4 bytes with value 0xFFFFFFFF to the MD5

    unsigned char md[EVP_MAX_MD_SIZE];
    unsigned int mdsz = std::size(md);
    EVP_DigestFinal(ctx, md, &mdsz);
    EVP_MD_CTX_free(ctx);

    // XXX: Revision 3 or greater only
    for (int i = 0; i < 50; i++) {
        EVP_Digest(md, mdsz, md, &mdsz, md5, NULL);
    }     

    return std::string(reinterpret_cast<char const*>(md), size_t(mdsz));
}

EncryptionContext makeEncryptionContext(User::EncryptionDict dict) {
    auto encO = dict.borrow_encO();
    auto encP = dict.borrow_encP();
    auto id0 = dict.borrow_id0();
    auto pwd = "";

    std::string key = makeFileKeyAlg2(encO, encP, id0, pwd);
    
    std::cerr << "MADE A KEY " ;
    for (auto &&x : key) { std::cerr << std::dec << unsigned((unsigned char)x) << " "; }
    std::cerr << std::endl;

    DDL::toJS(std::cerr, dict.borrow_ciph());
    std::cerr << std::endl;

    return EncryptionContext(key, dict.borrow_ciph());
}
