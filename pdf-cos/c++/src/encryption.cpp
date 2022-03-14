#include <vector>
#include <memory>

#include "digest.hpp"
#include "cipher.hpp"

#include <ddl/integer.h>
#include "main_parser.h"

#include "encryption.hpp"

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

std::vector<uint8_t> makeObjKey(
    EncryptionContext const& encCtx,
    uint64_t objId,
    uint16_t gen,
    bool isAES
) { 
    auto ctx = opensslxx::make_digest();

    ctx.init(EVP_md5());
    
    ctx.update(encCtx.key.data(), encCtx.key.size());
    
    // Specified to use 3 bytes of refid and 2 bytes of gen in little-endian encoding
    uint8_t idbytes[] = {
        uint8_t(objId >> (0*8)),
        uint8_t(objId >> (1*8)),
        uint8_t(objId >> (2*8)),
        uint8_t(gen   >> (0*8)),
        uint8_t(gen   >> (1*8)),
    };
    
    ctx.update(idbytes, std::size(idbytes));

    if (isAES) {
        const uint8_t salt[] = {0x73, 0x41, 0x6C, 0x54};
        ctx.update(salt, std::size(salt));
    }

    return ctx.final();
}

std::vector<uint8_t> makeFileKeyAlg2(
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
    
    auto ctx = opensslxx::make_digest();
    
    ctx.init(md5);
    ctx.update(password.data(), std::min(password.size(), size_t(32)));
    
    if (password.size() < 32) {
        ctx.update(padding, 32 - password.size());
    }

    std::vector<uint8_t> hashInput;
    hashInput.reserve(encO.size().value);
    for (DDL::Size i = 0; i < encO.size(); i.increment()) {
        hashInput.push_back(encO.borrowElement(i).rep());
    }
    ctx.update(hashInput.data(), hashInput.size());

    uint32_t pval = encP.asSize().value;
    uint8_t pbytes[4] = { uint8_t(pval >> (0*8)), uint8_t(pval >> (1*8)), uint8_t(pval >> (2*8)), uint8_t(pval >> (3*8)) };
    ctx.update(pbytes, std::size(pbytes));

    hashInput.clear();
    hashInput.reserve(id0.size().value);
    for (DDL::Size i = 0; i < id0.size(); i.increment()) {
        hashInput.push_back(id0.borrowElement(i).rep());
    }
    ctx.update(hashInput.data(), hashInput.size());

    // XXX: If document metadata is not being encrypted, pass 4 bytes with value 0xFFFFFFFF to the MD5

    auto md = ctx.final();

    // XXX: Revision 3 or greater only
    for (int i = 0; i < 50; i++) {
        md = opensslxx::digest(md5, md.data(), md.size());
    }

    return md;
}

EncryptionContext makeEncryptionContext(User::EncryptionDict dict) {
    auto encO = dict.borrow_encO();
    auto encP = dict.borrow_encP();
    auto id0 = dict.borrow_id0();
    auto pwd = "";

    auto key = makeFileKeyAlg2(encO, encP, id0, pwd);

    return EncryptionContext(key, dict.borrow_ciph());
}

bool aes_cbc_decryption(
    char const* input, size_t input_len,
    char const* key,
    std::string &output)
{
    if (input_len < 32 || input_len % 16) {
        return false;
    }

    output.resize(input_len - 16);

    auto ctx = opensslxx::make_cipher();

    ctx.init(EVP_aes_128_cbc(),
        reinterpret_cast<unsigned char const*>(key),
        reinterpret_cast<unsigned char const*>(input), // iv
        opensslxx::CipherDirection::decrypt
    );

    int outl = output.size();
    outl = ctx.update(
        reinterpret_cast<unsigned char *>(output.data()),
        outl,
        reinterpret_cast<unsigned char const*>(input+16),
        input_len - 16
    );

    ctx.final(nullptr, 0);

    if (!removePadding(output)) {
        output.clear();
        return false;
    }

    return true;
}
