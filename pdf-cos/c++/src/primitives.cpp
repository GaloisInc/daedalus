#include <iostream>
#include <cctype>
#include <main_parser.h>

#include <zlib.h>

#include "state.hpp"
#include "asciihex.hpp"
#include "ascii85.hpp"

bool parse_ResolveRef
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

  return references.resolve_reference(input, refid, gen, result);
}


bool parse_Decrypt
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input

  , DDL::Input body
  ) {

  *out_input = input;
  *result = body;
  return true;
}


bool parse_FlateDecode
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
  // XXX: Implement predictors

  predictor.free();
  colors.free();
  bpc.free();
  columns.free();
  auto bodyRef = owned(body);

  *out_input = input;

  std::vector<unsigned char> buffer;

  z_stream strm;
  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;
  strm.avail_in = input.length().value;
  strm.next_in = reinterpret_cast<unsigned char *>(bodyRef->borrowBytes());
  
  if (Z_OK != inflateInit(&strm)) {
    return false;
  }

  size_t const chunksize = 2048;

  do {
    size_t used = buffer.size();
    buffer.resize(used + chunksize);

    strm.avail_out = chunksize;
    strm.next_out = &buffer[used];

    int ret = inflate(&strm, Z_FINISH);

    switch (ret) {
      case Z_NEED_DICT:
      case Z_DATA_ERROR:
      case Z_MEM_ERROR:
        inflateEnd(&strm);
        return false;
    }

    if (strm.avail_out > 0) {
      buffer.resize(used + (chunksize - strm.avail_out));
    }
  } while (strm.avail_out == 0);

  inflateEnd(&strm);

  *result = DDL::Input("inflated", reinterpret_cast<char const*>(buffer.data()), DDL::Size(buffer.size()));

  return true;
}


bool parse_LZWDecode
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

  *out_input = input;
  std::cout << "XXX: parse_LZWDecode\n";
  return false;
}

bool parse_ASCIIHexDecode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  *out_input = input;
  auto bodyRef = owned(body);

  std::vector<unsigned char> buffer;
  
  if (ASCIIHexDecode(bodyRef->borrowBytes(), bodyRef->length().value, buffer)) {
    *result = DDL::Input("asciihex", reinterpret_cast<char*>(buffer.data()), DDL::Size(buffer.size()));
    return true;
  } else {
    return false;
  }
}

bool parse_ASCII85Decode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  *out_input = input;
  auto bodyRef = owned(body);

  std::vector<uint8_t> buffer;

  if (ASCII85Decode(bodyRef->borrowBytes(), bodyRef->length().value, buffer)) {
    *result = DDL::Input("ascii85", reinterpret_cast<char*>(buffer.data()), DDL::Size(buffer.size()));
    return true;
  } else {
    return false;
  }
}
