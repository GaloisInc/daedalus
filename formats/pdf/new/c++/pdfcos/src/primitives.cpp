#include <algorithm>
#include <iostream>
#include <iomanip>
#include <limits>
#include <openssl/evp.h>

#include <zlib.h>

#include "lzw.hpp"
#include "predictor.hpp"

#include <ddl/exception.h>
#include <pdfcos.hpp>

// owns inputin, message
DDL::ParserResult parser_Trace
  ( DDL::ParserStateUser<DDL::Input,ReferenceTable>& state
  , DDL::Unit* result
  , DDL::Input* inputout
  , DDL::Input inputin
  , DDL::Array<DDL::UInt<8ul>> message
  )
{
  *inputout = inputin;
  *result = DDL::Unit();

  std::string msg;
  msg.reserve(message.size().rep());

  for (DDL::Size i = 0; i < message.size(); i.increment()) {
    msg += message.borrowElement(i).rep();
  }
  message.free();

  std::cerr << "Parser trace: " << msg << std::endl;

  return DDL::ParserResult::Ok;
}


// owns input,ref
DDL::ParserResult parser_ResolveRef
  ( DDL::ParserStateUser<DDL::Input,ReferenceTable> &pstate
  , DDL::Maybe<PdfCos::TopDecl> *result
  , DDL::Input *out_input
  , DDL::Input input

  , PdfCos::Ref ref
  ) {

    uint64_t refid;
    generation_type gen;

    ref.borrow_obj().exportI(refid);
    ref.borrow_gen().exportI(gen);
    ref.free();

    auto &refs = pstate.getUserState();
    if (refs.resolve_reference(refid, gen, result)) {
      *out_input = input;
      return DDL::ParserResult::Ok;
    } else {
      input.free();
      return DDL::ParserResult::Failure;
    }
}

// owns input body
DDL::ParserResult parser_Decrypt
  ( DDL::ParserStateUser<DDL::Input,ReferenceTable> &pstate
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
              body.length().rep(),
              reinterpret_cast<char const*>(key.data()),
              output)
        ) {
          std::cerr << "INFO: Decryption has failed (v4AES)?" << std::endl;
          input.free();
          body.free();
          return DDL::ParserResult::Failure;
        }
        *result = DDL::Input("decrypted", output.data(), output.size());
        *out_input = input;
        body.free();
        return DDL::ParserResult::Ok;
      }
      case DDL::Tag::ChooseCiph::v5AES: {
        std::string output;
        if (!aes_cbc_decryption(
              EVP_aes_256_cbc(),
              body.borrowBytes().data(),
              body.length().rep(),
              reinterpret_cast<char const*>(refs.getEncryptionContext()->key.data()),
              output)
        ) {
          std::cerr << "INFO: Decryption has failed (v5AES)?" << std::endl;
          input.free();
          body.free();
          return DDL::ParserResult::Failure;
        }
        // Check length is multiple of 16 and longer than 0
        *result = DDL::Input("decrypted", output.data(), output.size());
        *out_input = input;
        body.free();
        return DDL::ParserResult::Ok;
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
        return DDL::ParserResult::Failure;
    }
  } else {
    *result = body;
    *out_input = input;
    return DDL::ParserResult::Ok;
  }
}

// owns input, predictor, colors, bpc, columns, body
DDL::ParserResult parser_FlateDecode
  ( DDL::ParserStateUser<DDL::Input,ReferenceTable> &pstate
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

    z_stream strm {};

    if (Z_OK != inflateInit(&strm)) {
      std::cerr << "INFO: inflate failed Z NOT OK" << std::endl;
      input.free();
      return DDL::ParserResult::Failure;
    }

    size_t const chunksize = 2048;
    auto const *compressed =
      reinterpret_cast<unsigned char const *>(bodyRef->borrowBytes().data());
    size_t const compressedSize = bodyRef->length().rep();
    size_t inputOffset = 0;

    for (;;) {
      if (strm.avail_in == 0 && inputOffset < compressedSize) {
        size_t const inputSize = std::min(
          compressedSize - inputOffset,
          size_t(std::numeric_limits<uInt>::max()));
        strm.avail_in = uInt(inputSize);
        strm.next_in = const_cast<unsigned char *>(compressed + inputOffset);
        inputOffset += inputSize;
      }

      size_t used = buffer.size();
      buffer.resize(used + chunksize);

      strm.avail_out = chunksize;
      strm.next_out = reinterpret_cast<unsigned char*>(&buffer[used]);

      int const status = inflate(&strm, Z_NO_FLUSH);
      buffer.resize(used + (chunksize - strm.avail_out));

      if (status == Z_STREAM_END) {
        break;
      }
      if (status != Z_OK) {
        std::cerr << "INFO: inflate failed";
        if (strm.msg != nullptr) {
          std::cerr << ": " << strm.msg;
        }
        std::cerr << std::endl;
        inflateEnd(&strm);
        input.free();
        return DDL::ParserResult::Failure;
      }
    }

    inflateEnd(&strm);

    if (!unpredict(
        predictor.asSize().rep(),
        colors.asSize().rep(),
        bpc.asSize().rep(),
        columns.asSize().rep(),
        buffer))
    {
      std::cerr << "INFO: unpredict failed" << std::endl;
      input.free();
      return DDL::ParserResult::Failure;
    }

    *result = DDL::Input("inflated", reinterpret_cast<char const*>(buffer.data()), DDL::Size(buffer.size()));
    *out_input = input;
    return DDL::ParserResult::Ok;
}


// owns input predictor colors bpc column earlychange body
DDL::ParserResult parser_LZWDecode
  ( DDL::ParserStateUser<DDL::Input,ReferenceTable> &pstate
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
  auto earlychangeOwned = DDL::Owned(earlychange);
  auto bodyRef = DDL::Owned(body);

  try {
    auto output = decompress(
        reinterpret_cast<uint8_t const*>(bodyRef->borrowBytes().data()),
        bodyRef->length().rep(),
        earlychange.asSize().rep());

    if (!unpredict(
        predictor.asSize().rep(),
        colors.asSize().rep(),
        bpc.asSize().rep(),
        columns.asSize().rep(),
        output))
    {
      input.free();
      return DDL::ParserResult::Failure;
    }

    *result = DDL::Input("lzw", output.data(), DDL::Size(output.length()));
    *out_input = input;
    return DDL::ParserResult::Ok;
  } catch (LzwException const& e) {
    std::cerr << "INFO: " << e.what() << std::endl;
    input.free();
    return DDL::ParserResult::Failure;
  }
}
