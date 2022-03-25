#include <main_parser.h>
#include "state.hpp"
#include "state.hpp"
#include "catalog.hpp"

void check_catalog() {
  auto root = references.getRoot();
  if (!root.has_value()) { throw CatalogException("Missing root"); }

  std::vector<User::PdfCatalog> results;
  DDL::ParseError error;

  parsePdfCatalog(error,results,DDL::Input("empty",""),true,root->get());

  if (results.size() != 1) {
    for (auto &&x : results) { x.free(); }
    throw CatalogException("Failed to parse catalog");
  }

  bool text = false; // true;

  if (text) {
    std::vector<DDL::ResultOf::parseTextInCatalog> chunks;
    parseTextInCatalog(error,chunks,DDL::Input("empty",""),results[0]);

    if (chunks.size() != 1) {
      for (auto &&x : results) { x.free(); }
      throw CatalogException("Failed to extract text");
    }

    auto p = chunks[0];

    // we free things as we go to avoid running out of stuck
    // in one big recursive free at the end.
    bool done = false;
    while (!done) {
      std::cout << std::endl;

      switch(p.getTag()) {
        case DDL::Tag::List::nil:
          p.free();
          done = true;
          break;

        case DDL::Tag::List::cons:
          auto node = p.borrow_cons();
          std::cout << node.borrow_head();
          auto q = node.get_tail();
          p.free();
          p = q;
      }
    }
    std::cerr << "\n";

  } else {
    // dbg << results[0] << std::endl;
    results[0].free();
  }
}
