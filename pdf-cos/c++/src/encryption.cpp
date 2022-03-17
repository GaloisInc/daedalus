#include <vector>
#include <memory>
#include <iomanip>

#include "digest.hpp"
#include "cipher.hpp"
#include "cryptoerror.hpp"

#include <ddl/integer.h>
#include "main_parser.h"

#include "encryption.hpp"

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

struct EncStringParts {
    uint8_t hashValue[32];
    uint8_t validationSalt[8];
    uint8_t keySalt[8];
};

namespace {
EncStringParts splitEncBytes(DDL::Array<DDL::UInt<8>> & input)
{
    if (input.size().value != 48) {
        throw EncryptionException();
    }

    EncStringParts result;

    for (DDL::Size i = 0; i < 32; i.increment()) {
        result.hashValue[i.value] = input[i].rep();
    }

    for (DDL::Size i = 0; i < 8; i.increment()) {
        result.validationSalt[i.value] = input[i].rep();
    }

    for (DDL::Size i = 0; i < 8; i.increment()) {
        result.keySalt[i.value] = input[i].rep();
    }

    return result;
}
}

std::vector<uint8_t> makeHash2b(
    std::vector<uint8_t> const& input,
    std::vector<uint8_t> const& extra
)
{
    auto k = opensslxx::digest(EVP_sha256(), input.data(), input.size());
    
    for (int iad = 0;; iad++) {
        std::vector<uint8_t> k1;
        
        for (int ia = 0; ia < 64; ia++) {
            std::copy(std::begin(input), std::end(input), std::back_inserter(k1));
            std::copy(std::begin(k), std::end(k), std::back_inserter(k1));
            std::copy(std::begin(extra), std::end(extra), std::back_inserter(k1));
        }

        // b
        auto cipher_b = opensslxx::make_cipher();
        cipher_b.set_padding(false);
        cipher_b.init(EVP_aes_128_cbc(), &k1[0], &k1[16], opensslxx::CipherDirection::encrypt);

        std::vector<uint8_t> e(k1.size() - 32);
        cipher_b.update(e.data(), e.size(), &k1[32], k1.size() - 32);

        int hash_choice = 0;
        for (size_t j = 0; j < 16; j++) {
            hash_choice += e[j];
        }
        EVP_MD const* md;
        switch (hash_choice % 3) {
            case 0: md = EVP_sha256(); break;
            case 1: md = EVP_sha384(); break;
            case 2: md = EVP_sha512(); break;
        }

        auto digest = opensslxx::make_digest();
        digest.init(md);
        digest.update(e.data(), e.size());
        k = digest.final();

        if (iad > 64 && e.back() <= (iad - 32)) break;
    }

    return k;
}

// 7.6.4.3.3 Algorithm 2.A: Retrieving the file encryption key from an encrypted document in order to decrypt it
// Revision 6 and later
std::vector<uint8_t> makeFileKeyAlg2a(
    DDL::Array<DDL::UInt<8>> encO,
    DDL::Integer encP,
    DDL::Array<DDL::UInt<8>> encU,
    DDL::Array<DDL::UInt<8>> encOE,
    DDL::Array<DDL::UInt<8>> encUE,
    DDL::Array<DDL::UInt<8>> id0,
    std::string password
){
    // a) SASLprep - assumed to be complete already

    // b) Truncate password to 127 bytes
    if (password.size() > 127) password.resize(127);

    // c) test password against owner key
    auto ownerParts = splitEncBytes(encO);

    throw "incomplete";
}

// Revision 4 and earlier
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
        auto x = opensslxx::digest(md5, md.data(), md.size());
        md = x;
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
    EVP_CIPHER const* alg,
    char const* input, size_t input_len,
    char const* key,
    std::string &output)
{
    if (input_len < 32 || input_len % 16) {
        return false;
    }

    output.resize(input_len - 16);

    try {
        auto ctx = opensslxx::make_cipher();

        ctx.init(alg,
            reinterpret_cast<unsigned char const*>(key),
            reinterpret_cast<unsigned char const*>(input), // iv
            opensslxx::CipherDirection::decrypt
        );

        unsigned char * cursor = reinterpret_cast<unsigned char *>(output.data());
        int out_avail = output.size();

        int outl = ctx.update(
            cursor, out_avail,
            reinterpret_cast<unsigned char const*>(input+16),
            input_len - 16
        );
        cursor += outl;
        out_avail -= outl;

        int outf = ctx.final(cursor, out_avail);
        cursor += outf;
        out_avail -= outf;

        output.resize(output.size() - out_avail);

    } catch (opensslxx::OpenSSLXX_exception const& e) {
        std::cerr << "OpenSLL failed: " << e.code << std::endl;
        return false;
    }

    return true;
}
