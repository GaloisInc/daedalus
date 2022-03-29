#include <main_parser.h>
#include "state.hpp"
#include "state.hpp"
#include "catalog.hpp"

void check_catalog(bool text) {
  auto root = references.getRoot();
  if (!root.has_value()) { throw CatalogException("Missing root"); }

  std::vector<User::PdfCatalog> results;
  DDL::ParseError error;

  parsePdfCatalog(error,results,DDL::Input("empty",""),true,root->get());

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
