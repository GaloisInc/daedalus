#include <iostream>
#include <cctype>
#include <iomanip>
#include <main_parser.h>
#include <openssl/evp.h>

#include <zlib.h>

#include "state.hpp"
#include "asciihex.hpp"
#include "ascii85.hpp"
#include "lzw.hpp"
#include "predictor.hpp"
#include "encryption.hpp"

bool parser_Trace
  ( DDL::ParserState& state
  , DDL::Unit* result
  , DDL::Input* inputout
  , DDL::Input inputin
  , DDL::Array<DDL::UInt<8ul>> message
  )
{
  *inputout = inputin;
  *result = DDL::Unit();

  std::string msg;
  for (DDL::Size i = 0; i < message.size(); i.increment()) {
    msg += message.borrowElement(i).rep();
  }

  std::cerr << "Parser trace: " << msg << std::endl;

  return true;
}

bool parser_ResolveRef
  ( DDL::ParserState &pstate
  , DDL::Maybe<User::TopDecl> *result
  , DDL::Input *out_input
  , DDL::Input input

  , User::Ref ref
  ) {

  *out_input = input;

  // XXX: bounds checking
  uint64_t refid = ref.borrow_obj().asSize().value;
  uint16_t gen = ref.borrow_gen().asSize().value;
  return references.resolve_reference(refid, gen, result);
}

void debug_print(char const* label, char const* data, size_t len)
{
  std::cerr << label << ":";
  for (size_t i = 0; i < len; i++) {
    std::cerr << " " << std::hex << std::setfill('0') << std::setw(2) << unsigned((unsigned char)data[i]);
  }
  std::cerr << std::endl;
}

bool parser_Decrypt
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input

  , DDL::Input body
  ) {

  if (references.getEncryptionContext().has_value()) {
    auto const& e = *references.getEncryptionContext();
    
    switch (e.cipher.borrow().getTag()) {
      case DDL::Tag::ChooseCiph::v4AES: {
        auto key = makeObjKey(
          *references.getEncryptionContext(),
          references.currentObjId,
          references.currentGen,
          true);

        std::string output;
        if (!aes_cbc_decryption(
              EVP_aes_128_cbc(),
              body.borrowBytes(),
              body.length().value,
              reinterpret_cast<char const*>(key.data()),
              output)
        ) {
          std::cerr << "Decryption has failed?" << std::endl;
          return false;
        }
        // Check length is multiple of 16 and longer than 0

        body.free();

        *result = DDL::Input("decrypted", output.data(), output.size());
        *out_input = input;
        return true;
      }
      case DDL::Tag::ChooseCiph::v5AES: {
        std::string output;
        if (!aes_cbc_decryption(
              EVP_aes_256_cbc(),
              body.borrowBytes(),
              body.length().value,
              reinterpret_cast<char const*>(references.getEncryptionContext()->key.data()),
              output)
        ) {
          std::cerr << "Decryption has failed?" << std::endl;
          return false;
        }
        // Check length is multiple of 16 and longer than 0

        body.free();

        *result = DDL::Input("decrypted", output.data(), output.size());
        *out_input = input;
        return true;
      }
      default:
        std::cerr
          << "Encryption not implemented "
          << references.currentObjId << " "
          << references.currentGen
          << std::endl;
        body.free();
        input.free();
        return false;
    }
  } else {
    *result = body;
    *out_input = input;
    return true;
  }
}

bool parser_FlateDecode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input

  , DDL::Integer predictor
  , DDL::Integer colors
  , DDL::Integer bpc
  , DDL::Integer columns
  , DDL::Input   body
  ) {

    auto predictorOwned = owned(predictor);
    auto colorsOwned = owned(colors);
    auto bpcOwned = owned(bpc);
    auto columnsOwned = owned(columns);
    auto bodyRef = owned(body);

    std::string buffer;

    z_stream strm;
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    strm.avail_in = body.length().value;
    strm.next_in = reinterpret_cast<unsigned char *>(bodyRef->borrowBytes());
    
    if (Z_OK != inflateInit(&strm)) {
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
              std::cerr << "inflate failed" << std::endl;
          input.free();
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
      return false;
    }

    *result = DDL::Input("inflated", reinterpret_cast<char const*>(buffer.data()), DDL::Size(buffer.size()));
    *out_input = input;

    return true;
}

bool parser_LZWDecode
  ( DDL::ParserState &pstate
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

  auto bodyRef = owned(body);

  // XXX: support predictors
  predictor.free();
  colors.free();
  bpc.free();
  columns.free();
  earlychange.free();

  try {
    auto output = decompress(reinterpret_cast<uint8_t const*>(bodyRef->borrowBytes()), bodyRef->length().value);

    if (!unpredict(
        predictor.asSize().value,
        colors.asSize().value,
        bpc.asSize().value,
        columns.asSize().value,
        output))
    {
      return false;
    }

    *result = DDL::Input("lzw", output.data(), DDL::Size(output.length()));
    *out_input = input;
    return true;
  } catch (LzwException const& e) {
    std::cerr << e.what() << std::endl;
    input.free();
    return false;
  }
}

bool parser_ASCIIHexDecode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  auto bodyRef = owned(body);

  std::vector<unsigned char> buffer;
  
  if (ASCIIHexDecode(bodyRef->borrowBytes(), bodyRef->length().value, buffer)) {
    *result = DDL::Input("asciihex", reinterpret_cast<char*>(buffer.data()), DDL::Size(buffer.size()));
    *out_input = input;
    return true;
  } else {
    input.free();
    return false;
  }
}

bool parser_ASCII85Decode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  auto bodyRef = owned(body);

  std::vector<uint8_t> buffer;

  if (ASCII85Decode(bodyRef->borrowBytes(), bodyRef->length().value, buffer)) {
    *result = DDL::Input("ascii85", reinterpret_cast<char*>(buffer.data()), DDL::Size(buffer.size()));
    *out_input = input;
    return true;
  } else {
    std::cerr << "ascii85 failed" << std::endl;
    input.free();
    return false;
  }
}
