#include <main_parser.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include "state.hpp"
#include "state.hpp"
#include "catalog.hpp"

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



bool getGlyphMap(const char *file, DDL::ResultOf::parseStdEncodings *out) {

  DDL::Input input;
  if (!inputFromFile(file, &input)) {
    std::cerr << "Unable to open glyph file " << file << std::endl;
    return false;
  }

  std::vector<DDL::ResultOf::parseStdEncodings> results;
  DDL::ParseError err;
  parseStdEncodings(err,results,input);
  if (results.size() != 1) {
    for (auto &&x : results) { x.free(); }
    std::cerr << "Unable to parse glyph file " << file << std::endl;
    return false;
  }

  *out = results[0];
  return true;
}



void check_catalog(bool text) {
  auto root = references.getRoot();
  if (!root.has_value()) { throw CatalogException("Missing root"); }

  std::vector<User::PdfCatalog> results;
  DDL::ParseError error;

  DDL::Maybe <DDL::ResultOf::parseStdEncodings> mbglyphs;

  if (text) {
    DDL::ResultOf::parseStdEncodings glyphs;
    if (!getGlyphMap("glyphs.txt",&glyphs))
      throw CatalogException("Failed to parse glyph file.");
    mbglyphs = DDL::Maybe {glyphs};
  }

  parsePdfCatalog(error,results,DDL::Input("empty",""),true,mbglyphs,root->get());

  if (results.size() != 1) {
    for (auto &&x : results) { x.free(); }
    throw CatalogException("Failed to parse catalog");
  }

  if (text) {

    std::vector<DDL::ResultOf::parseTextInCatalog> chunks;
    parseTextInCatalog(error,chunks,DDL::Input("empty",""),results[0]);
  } else {
    // dbg << results[0] << std::endl;
    results[0].free();
  }
}
