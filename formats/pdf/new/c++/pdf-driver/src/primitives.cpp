#include <iostream>
#include <cctype>
#include <iomanip>
#include <main_parser.h>
#include <pdfcos.hpp>


#include "debug.hpp"

std::u32string emittedCodepoints;


bool parser_GetCharCode
  ( DDL::ParserStateUser<DDL::Input,ReferenceTable>& state
  , DDL::SInt<32> *result
  , DDL::Input    *inputout
  , DDL::Input inputin // own
  , PdfDriver::cmap cmap //own
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
  ( DDL::ParserStateUser<DDL::Input,ReferenceTable>& state
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


