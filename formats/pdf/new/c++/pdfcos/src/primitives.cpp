#include <iostream>
#include <cctype>
#include <iomanip>
#include <openssl/evp.h>

#include <zlib.h>

#include "asciihex.hpp"
#include "ascii85.hpp"
#include "lzw.hpp"
#include "predictor.hpp"

#include <pdfcos.h>
#include "pdfcos/debug.hpp"
#include "pdfcos/state.hpp"
#include "pdfcos/encryption.hpp"

// owns inputin, message
bool parser_Trace
  ( DDL::ParserStateUser<ReferenceTable>& state
  , DDL::Unit* result
  , DDL::Input* inputout
  , DDL::Input inputin
  , DDL::Array<DDL::UInt<8ul>> message
  )
{
  *inputout = inputin;
  *result = DDL::Unit();

  std::string msg;
  msg.reserve(message.size().value);

  for (DDL::Size i = 0; i < message.size(); i.increment()) {
    msg += message.borrowElement(i).rep();
  }
  message.free();

  std::cerr << "Parser trace: " << msg << std::endl;

  return true;
}


// owns input,ref
bool parser_ResolveRef
  ( DDL::ParserStateUser<ReferenceTable> &pstate
  , DDL::Maybe<User::TopDecl> *result
  , DDL::Input *out_input
  , DDL::Input input

  , User::Ref ref
  ) {

    uint64_t refid;
    generation_type gen;

    ref.borrow_obj().exportI(refid);
    ref.borrow_gen().exportI(gen);
    ref.free();

    auto &refs = pstate.getUserState();
    if (refs.resolve_reference(refid, gen, result)) {
      *out_input = input;
      return true;
    } else {
      input.free();
      return false;
    }
}

// owns input body
bool parser_Decrypt
  ( DDL::ParserStateUser<ReferenceTable> &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input

  , DDL::Input body
  ) {

  auto &refs = pstate.getUserState();
  if (refs.getEncryptionContext().has_value()) {
    auto const& e = *refs.getEncryptionContext();
    
    switch (e.cipher.borrow().getTag()) {
      case DDL::Tag::ChooseCiph::v4AES: {
        auto key = makeObjKey(
          *refs.getEncryptionContext(),
          refs.currentObjId,
          refs.currentGen,
          true);

        std::string output;
        if (!aes_cbc_decryption(
              EVP_aes_128_cbc(),
              body.borrowBytes().data(),
              body.length().value,
              reinterpret_cast<char const*>(key.data()),
              output)
        ) {
          std::cerr << "INFO: Decryption has failed (v4AES)?" << std::endl;
          input.free();
          body.free();
          return false;
        }
        *result = DDL::Input("decrypted", output.data(), output.size());
        *out_input = input;
        body.free();
        return true;
      }
      case DDL::Tag::ChooseCiph::v5AES: {
        std::string output;
        if (!aes_cbc_decryption(
              EVP_aes_256_cbc(),
              body.borrowBytes().data(),
              body.length().value,
              reinterpret_cast<char const*>(refs.getEncryptionContext()->key.data()),
              output)
        ) {
          std::cerr << "INFO: Decryption has failed (v5AES)?" << std::endl;
          input.free();
          body.free();
          return false;
        }
        // Check length is multiple of 16 and longer than 0
        *result = DDL::Input("decrypted", output.data(), output.size());
        *out_input = input;
        body.free();
        return true;
      }
      default:
        std::cerr
          << "INFO: Encryption not implemented. "
          << "Object: "
          << refs.currentObjId << " "
          << refs.currentGen
          << ", cipher: " << e.cipher.borrow()
          << std::endl;
        input.free();
        body.free();
        return false;
    }
  } else {
    *result = body;
    *out_input = input;
    return true;
  }
}

// owns input, predictor, colors, bpc, columns, body
bool parser_FlateDecode
  ( DDL::ParserStateUser<ReferenceTable> &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input

  , DDL::Integer predictor
  , DDL::Integer colors
  , DDL::Integer bpc
  , DDL::Integer columns
  , DDL::Input   body
  ) {

    auto predictorOwned = DDL::Owned(predictor);
    auto colorsOwned = DDL::Owned(colors);
    auto bpcOwned = DDL::Owned(bpc);
    auto columnsOwned = DDL::Owned(columns);
    auto bodyRef = DDL::Owned(body);

    std::string buffer;

    z_stream strm;
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = body.length().value;
    strm.next_in = const_cast<unsigned char *>(reinterpret_cast<unsigned char const *>(bodyRef->borrowBytes().data()));
    
    if (Z_OK != inflateInit(&strm)) {
      std::cerr << "INFO: inflate failed Z NOT OK" << std::endl;
      input.free();
      return false;
    }

    size_t const chunksize = 2048;

    do {
      size_t used = buffer.size();
      buffer.resize(used + chunksize);

      strm.avail_out = chunksize;
      strm.next_out = reinterpret_cast<unsigned char*>(&buffer[used]);

      int ret = inflate(&strm, Z_FINISH);

      switch (ret) {
        case Z_NEED_DICT:
        case Z_DATA_ERROR:
        case Z_MEM_ERROR:
          inflateEnd(&strm);
          input.free();
          std::cerr << "INFO: inflate failed" << std::endl;
          return false;
      }

      if (strm.avail_out > 0) {
        buffer.resize(used + (chunksize - strm.avail_out));
      }
    } while (strm.avail_out == 0);

    inflateEnd(&strm);

    if (!unpredict(
        predictor.asSize().value,
        colors.asSize().value,
        bpc.asSize().value,
        columns.asSize().value,
        buffer))
    {
      std::cerr << "INFO: unpredict failed" << std::endl;
      input.free();
      return false;
    }

    *result = DDL::Input("inflated", reinterpret_cast<char const*>(buffer.data()), DDL::Size(buffer.size()));
    *out_input = input;
    return true;
}


// owns input predictor colors bpc column earlychange body
bool parser_LZWDecode
  ( DDL::ParserStateUser<ReferenceTable> &pstate
  , DDL::Input* result
  , DDL::Input* out_input
  , DDL::Input input

  , DDL::Integer predictor
  , DDL::Integer colors
  , DDL::Integer bpc
  , DDL::Integer columns
  , DDL::Integer earlychange
  , DDL::Input body
  ) {

  auto predictorOwned = DDL::Owned(predictor);
  auto colorsOwned = DDL::Owned(colors);
  auto bpcOwned = DDL::Owned(bpc);
  auto columnsOwned = DDL::Owned(columns);
  auto bodyRef = DDL::Owned(body);

  try {
    auto output = decompress(reinterpret_cast<uint8_t const*>(bodyRef->borrowBytes().data()), bodyRef->length().value);

    if (!unpredict(
        predictor.asSize().value,
        colors.asSize().value,
        bpc.asSize().value,
        columns.asSize().value,
        output))
    {
      input.free();
      return false;
    }

    if (!unpredict(
        predictor.asSize().value,
        colors.asSize().value,
        bpc.asSize().value,
        columns.asSize().value,
        output))
    {
      input.free();
      return false;
    }

    *result = DDL::Input("lzw", output.data(), DDL::Size(output.length()));
    *out_input = input;
    return true;
  } catch (LzwException const& e) {
    std::cerr << "INFO: " << e.what() << std::endl;
    input.free();
    return false;
  }
}

// owns input,body
bool parser_ASCIIHexDecode
  ( DDL::ParserStateUser<ReferenceTable> &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  auto bodyRef = DDL::Owned(body);

  std::vector<unsigned char> buffer;
  
  if (ASCIIHexDecode(bodyRef->borrowBytes().data(), bodyRef->length().value, buffer)) {
    *result = DDL::Input("asciihex", reinterpret_cast<char*>(buffer.data()), DDL::Size(buffer.size()));
    *out_input = input;
    return true;
  } else {
    input.free();
    return false;
  }
}

// owns input,body
bool parser_ASCII85Decode
  ( DDL::ParserStateUser<ReferenceTable> &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  auto bodyRef = DDL::Owned(body);

  std::vector<uint8_t> buffer;

  if (ASCII85Decode(bodyRef->borrowBytes().data(), bodyRef->length().value, buffer)) {
    *result = DDL::Input("ascii85", reinterpret_cast<char*>(buffer.data()), DDL::Size(buffer.size()));
    *out_input = input;
    return true;
  } else {
    std::cerr << "INFO: ascii85 failed" << std::endl;
    input.free();
    return false;
  }
}
