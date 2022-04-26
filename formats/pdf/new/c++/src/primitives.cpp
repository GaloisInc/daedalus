#include <iostream>
#include <cctype>
#include <iomanip>
#include <main_parser.h>
#include <openssl/evp.h>

#include <zlib.h>

#include "debug.hpp"
#include "state.hpp"
#include "asciihex.hpp"
#include "ascii85.hpp"
#include "lzw.hpp"
#include "predictor.hpp"
#include "encryption.hpp"

std::u32string emittedCodepoints;


bool parser_GetCharCode
  ( DDL::ParserState& state
  , DDL::SInt<32> *result
  , DDL::Input    *inputout
  , DDL::Input inputin // own
  , User::cmap cmap //own
  ) {

  if (inputin.isEmpty()) {
    cmap.free();
    inputin.free();
    return false;
  }


  auto ranges = cmap.borrow_ranges();
  auto len    = ranges.size().rep();
  bool consider[len];
  for (size_t i = 0; i < len; ++i) consider[i] = true;

  if (len == 0) {
    std::cerr << "Info: EMPTY RANGE\n";
    cmap.free();
    inputin.free();
    return false;
  }

  int32_t val = 0;
  auto inp = inputin;
  for (size_t byteIx = 0; byteIx < 4; ++byteIx) {
    if (inp.isEmpty()) {
      // std::cerr << "early end of file " << ranges << "\n";
      goto FAIL;
      // *inputout = inp;
      // *result = val;
      // cmap.free();
      return true;
    } 
    auto b = inp.iHead();
    // std::cerr << "byteIx = " << byteIx << ", byte = " << std::hex << b << "\n";
    val    = (val << 8) | b.rep();
    inp.iDropMut(DDL::Size{1});

    bool anyOK = false;
    for (size_t i = 0; i < len; ++i) {
      if (!consider[i]) continue;
      auto r = ranges.borrowElement(DDL::Size{i});
      auto start = r.borrow_start();

      // Here we know that that we are in the the domain of the array
      // because if we matched the previous byte and it was the last one
      // we would have returned.  See below.
      auto lower = start.borrowElement(DDL::Size{byteIx});
      //std::cerr << "lower = " << lower << "\n";
      if (b < lower) { consider[i] = false; continue; }

      // assumes start and end have the same length.
      auto upper = r.borrow_end().borrowElement(DDL::Size{byteIx});
      //std::cerr << "upper = " << upper << "\n";
      if (b > upper) { consider[i] = false; continue; }
      // std::cerr << "matched, byteIx = " << byteIx << ", size = " << start.size() << std::endl;

      anyOK = true;

      if ((byteIx + 1) == start.size().rep()) {
        // std::cerr << "OK = " << val << "\n";
        *inputout = inp;
        *result = val;
        cmap.free();
        return true;
      }
    }
    // std::cerr << "next byte\n";
    // if (!anyOK) goto FAIL;
  }

FAIL:
  // std::cerr << "No code space\n";
  // std::cerr << cmap.borrow_ranges() << std::endl;
  *inputout = inp;
  *result = -1;
  cmap.free();
  return true;
}

bool parser_EmitChar
  ( DDL::ParserState& state
  , DDL::Unit* result
  , DDL::Input *inputout
  , DDL::Input inputin
  , DDL::UInt<32> c
  ) {
  emittedCodepoints.push_back(c.rep());

  *inputout = inputin;
  *result   = DDL::Unit();
  return true;
}


// owns inputin, message
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
  ( DDL::ParserState &pstate
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

    if (references.resolve_reference(refid, gen, result)) {
      *out_input = input;
      return true;
    } else {
      input.free();
      return false;
    }
}

// owns input body
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
              body.borrowBytes(),
              body.length().value,
              reinterpret_cast<char const*>(references.getEncryptionContext()->key.data()),
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
          << references.currentObjId << " "
          << references.currentGen
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
    strm.next_in = reinterpret_cast<unsigned char *>(bodyRef->borrowBytes());
    
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

  auto predictorOwned = DDL::Owned(predictor);
  auto colorsOwned = DDL::Owned(colors);
  auto bpcOwned = DDL::Owned(bpc);
  auto columnsOwned = DDL::Owned(columns);
  auto bodyRef = DDL::Owned(body);

  try {
    auto output = decompress(reinterpret_cast<uint8_t const*>(bodyRef->borrowBytes()), bodyRef->length().value);

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
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  auto bodyRef = DDL::Owned(body);

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

// owns input,body
bool parser_ASCII85Decode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  auto bodyRef = DDL::Owned(body);

  std::vector<uint8_t> buffer;

  if (ASCII85Decode(bodyRef->borrowBytes(), bodyRef->length().value, buffer)) {
    *result = DDL::Input("ascii85", reinterpret_cast<char*>(buffer.data()), DDL::Size(buffer.size()));
    *out_input = input;
    return true;
  } else {
    std::cerr << "INFO: ascii85 failed" << std::endl;
    input.free();
    return false;
  }
}
