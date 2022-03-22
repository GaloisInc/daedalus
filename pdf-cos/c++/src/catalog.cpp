#include <main_parser.h>
#include "state.hpp"
#include "state.hpp"

void check_catalog() {
  auto root = references.getRoot();
  if (!root.has_value()) {
    std::cerr << "Missing root\n";
    return;
  }

  std::vector<User::PdfCatalog> results;
  DDL::ParseError error;

  parsePdfCatalog(error,results,DDL::Input("empty",""),root->get());

  if (results.size() != 1) {
    for (auto &&x : results) { x.free(); }
    std::cerr << "Failed to parse catalog\n";
    return;
  }

  std::cerr << results[0] << std::endl;
  results[0].free();
}
