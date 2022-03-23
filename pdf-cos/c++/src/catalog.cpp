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

  parsePdfCatalog(error,results,DDL::Input("empty",""),true,root->get());

  if (results.size() != 1) {
    for (auto &&x : results) { x.free(); }
    std::cerr << "Failed to parse catalog\n";
    return;
  }

  bool text = false;

  if (text) {
    std::vector<DDL::ResultOf::parseTextInCatalog> chunks;
    parseTextInCatalog(error,chunks,DDL::Input("empty",""),results[0]);

    if (chunks.size() != 1) {
      for (auto &&x : results) { x.free(); }
      std::cerr << "Failed to extract text\n";
      return;
    }

    auto p = chunks[0];

    bool done = false;
    while (!done) {
      switch(p.getTag()) {
        case DDL::Tag::List::nil:
          done = true;
          break;

        case DDL::Tag::List::cons:
          auto node = p.borrow_cons();
          std::cerr << (char*)node.borrow_head().borrowData();
          p = node.borrow_tail();
      }
    }
    std::cerr << "\n";

    chunks[0].free();
  } else {
    std::cerr << results[0] << std::endl;
    results[0].free();
  }
}
