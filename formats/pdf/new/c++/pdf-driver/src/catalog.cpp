#include <main_parser.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include "catalog.hpp"
#include "glyphmap.h"

bool inputFromFile(const char *file, DDL::Input *input)
{
  std::ifstream fin {file, std::ios::in | std::ios::binary};
  std::ostringstream sout;

  if (!fin.is_open()) {
    return false;
  }

  sout << fin.rdbuf();
  std::string str = std::move(sout.str());

  *input = DDL::Input{file, str.data(), str.size()};
  return true;
}



// XXX: Tihs is a differen format and should not need the referneces
bool getGlyphMap(ReferenceTable &refs, const char *file, DDL::ResultOf::parseStdEncodings *out) {

  DDL::Input input{"glyphmap.txt",(const char*)glyphmap_txt,glyphmap_txt_len};

  std::vector<DDL::ResultOf::parseStdEncodings> results;
  DDL::ParseError<DDL::Input> err;
  parseStdEncodings(refs, err,results,input);
  if (results.size() != 1) {
    for (auto &&x : results) { x.free(); }
    std::cerr << "Unable to parse glyph file " << file << std::endl;
    return false;
  }

  *out = results[0];
  return true;
}



void check_catalog(ReferenceTable &refs, bool text) {
  auto root = refs.getRoot();
  if (!root.has_value()) { throw CatalogException("Missing root"); }

  std::vector<PdfDriver::PdfCatalog> results;
  DDL::ParseError<DDL::Input> error;


  DDL::Maybe <DDL::ResultOf::parseStdEncodings> mbglyphs;

  if (text) {
    DDL::ResultOf::parseStdEncodings glyphs;
    if (!getGlyphMap(refs, "glyphs.txt",&glyphs))
      throw CatalogException("Failed to parse glyph file.");
    mbglyphs = DDL::Maybe {glyphs};
  }

  parsePdfCatalog(refs, error,results,DDL::Input("empty",""),true,mbglyphs,root->get());

  if (results.size() != 1) {
    for (auto &&x : results) { x.free(); }
    throw CatalogException("Failed to parse catalog");
  }

  if (text) {

    std::vector<DDL::ResultOf::parseTextInCatalog> chunks;
    parseTextInCatalog(refs, error,chunks,DDL::Input("empty",""),results[0]);
  } else {
    // dbg << results[0] << std::endl;
    results[0].free();
  }
}
