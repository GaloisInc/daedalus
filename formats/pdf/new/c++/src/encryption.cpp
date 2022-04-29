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

    for (DDL::Size i = 32; i < 40; i.increment()) {
        result.validationSalt[i.value-32] = input[i].rep();
    }

    for (DDL::Size i = 40; i < 48; i.increment()) {
        result.keySalt[i.value-40] = input[i].rep();
    }

    return result;
}
}

std::vector<uint8_t> makeHash2b(
    std::vector<uint8_t> const& password,
    std::vector<uint8_t> const& salt,
    std::vector<uint8_t> const& udata
)
{
    // Round 0
    auto digest0 = opensslxx::make_digest();
    digest0.init(EVP_sha256());
    digest0.update(password.data(), password.size());
    digest0.update(salt.data(), salt.size());
    digest0.update(udata.data(), udata.size());
    auto K = digest0.final();

    // If R < 6: return K;

    for (int round_num = 1;; round_num++) {
        std::vector<uint8_t> K1;
        for (int ia = 0; ia < 64; ia++) {
            std::copy(std::begin(password), std::end(password), std::back_inserter(K1));
            std::copy(std::begin(K), std::end(K), std::back_inserter(K1));
            std::copy(std::begin(udata), std::end(udata), std::back_inserter(K1));
        }

        // b
        std::vector<uint8_t> E(K1.size());
        auto cipher_b = opensslxx::make_cipher();
        cipher_b.init(EVP_aes_128_cbc(), &K1[0], &K1[16], opensslxx::CipherDirection::encrypt);
        cipher_b.set_padding(false);
        cipher_b.update(E.data(), E.size(), K1.data(), K1.size());
        cipher_b.final(nullptr, 0);

        // c
        int hash_choice = 0;
        for (size_t j = 0; j < 16; j++) {
            hash_choice += E[j];
        }
        EVP_MD const* md;
        switch (hash_choice % 3) {
            case 0: md = EVP_sha256(); break;
            case 1: md = EVP_sha384(); break;
            case 2: md = EVP_sha512(); break;
        }

        // d
        auto digest = opensslxx::make_digest();
        digest.init(md);
        digest.update(E.data(), E.size());
        K = digest.final();

        if (round_num >= 64 && E.back() <= round_num - 32) {
            K.resize(32);
            return K;
        }
    }
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
    std::vector<uint8_t> const& password
){
    // a) SASLprep - assumed to be complete already

    // b) Truncate password to 127 bytes
    //if (password.size() > 127) password.resize(127);

    // c) test password against owner key
    auto ownerParts = splitEncBytes(encO);

    throw "incomplete";
}

namespace {
std::vector<uint8_t> arrayToVector(DDL::Array<DDL::UInt<8>> array) {
    std::vector<uint8_t> result;
    result.reserve(array.size().value);

    for (DDL::Size i = 0; i < array.size(); i.increment()) {
        result.push_back(array.borrowElement(i).rep());
    }

    return result;
}
}

// Revision 4 and earlier
std::vector<uint8_t> makeFileKeyAlg2(
    DDL::Array<DDL::UInt<8>> encO,
    DDL::Integer encP,
    DDL::Array<DDL::UInt<8>> id0,
    std::vector<uint8_t> const& password
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
    auto pwd = std::vector<uint8_t>();

    uint8_t encV;
    dict.borrow_encV().exportI(encV);

    if (encV < 5) {
        auto key = makeFileKeyAlg2(encO, encP, id0, pwd);
        return EncryptionContext(key, dict.borrow_ciph());
    } else {
        auto encO = dict.borrow_encO();
        auto encU = dict.borrow_encU();

        auto owner = splitEncBytes(encO);
        auto user = splitEncBytes(encU);
        auto userBytes = arrayToVector(encU);

        std::vector<uint8_t> pwdBytes;

        std::vector<uint8_t> encUBytes = arrayToVector(dict.borrow_encU());

        auto ovs = std::vector(std::begin(owner.validationSalt), std::end(owner.validationSalt));
        auto step_c = makeHash2b(pwdBytes, ovs, encUBytes);

        if (std::equal(std::begin(step_c), std::end(step_c), std::begin(owner.hashValue))) {
            std::cerr << "Password is OWNER password\n";

            if (dict.borrow_encOE().isNothing()) {
                throw EncryptionException();
            }

            auto oks = std::vector(std::begin(owner.keySalt), std::end(owner.keySalt));
            auto step_d = makeHash2b(pwdBytes, oks, encUBytes);
            auto oeBytes = arrayToVector(dict.borrow_encOE().borrowValue());

            auto cipher = opensslxx::make_cipher();
            std::vector<uint8_t> iv(16, 0);
            std::vector<uint8_t> filekey(oeBytes.size());
            cipher.init(EVP_aes_256_cbc(), step_d.data(), iv.data(), opensslxx::CipherDirection::decrypt);
            cipher.set_padding(false);
            cipher.update(filekey.data(), filekey.size(), oeBytes.data(), oeBytes.size());
            cipher.final(nullptr, 0);

            return EncryptionContext(filekey, dict.borrow_ciph());
        } else {
            throw EncryptionException();
        }
    }
}

bool aes_cbc_decryption(
    EVP_CIPHER const* alg,
    char const* input, size_t input_len,
    char const* key,
    std::string &output)
{
    if (input_len < 32 || input_len % 16) {
        std::cerr << "[ERROR] aes_cbc_decryption unexpected input_len=" << input_len << std::endl;
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
        std::cerr << "[ERROR] OpenSSL failed: " << e.code << std::endl;
        return false;
    }

    return true;
}
