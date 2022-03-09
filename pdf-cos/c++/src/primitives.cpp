#include <iostream>
#include <main_parser.h>

#include <zlib.h>

#include "state.hpp"

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
  std::vector<unsigned char> chunk(512);

  z_stream strm;
  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;
  strm.avail_in = input.length().value;
  strm.next_in = reinterpret_cast<unsigned char *>(bodyRef->borrowBytes());
  
  if (Z_OK != inflateInit(&strm)) {
    return false;
  }

  do {
    strm.avail_out = chunk.size();
    strm.next_out = chunk.data();

    int ret = inflate(&strm, Z_FINISH);

    switch (ret) {
      case Z_NEED_DICT:
      case Z_DATA_ERROR:
      case Z_MEM_ERROR:
        inflateEnd(&strm);
        return false;
    }

    std::copy_n(std::begin(chunk), chunk.size() - strm.avail_out, std::back_inserter(buffer));
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
  std::cout << "XXX: parse_ASCIIHexDecode\n";
  return false;
}

bool parse_ASCII85Decode
  ( DDL::ParserState &pstate
  , DDL::Input *result
  , DDL::Input *out_input
  , DDL::Input input
  , DDL::Input body
  ) {

  *out_input = input;
  std::cout << "XXX: parse_ASCII85Decode\n";
  return false;
}


