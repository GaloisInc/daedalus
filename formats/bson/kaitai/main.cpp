#include <iostream>
#include <fstream>
#include "bson.h"


size_t str_size(bson_t::string_t *str) {
  return str->str().length();
}

size_t doc_size(bson_t *doc) {
  size_t s = 0;
  for (auto i : *doc->fields()->elements()) {
    auto k = i->content();
    using N = bson_t::element_t;
    switch (i->type_byte()) {
      case N::BSON_TYPE_STRING: s += str_size((bson_t::string_t*)k); break;
      case N::BSON_TYPE_DOCUMENT: s += doc_size((bson_t*)k); break;
      case N::BSON_TYPE_ARRAY: s += doc_size((bson_t*)k); break;
      case N::BSON_TYPE_JAVASCRIPT: s += str_size((bson_t::string_t*)k); break;
      case N::BSON_TYPE_SYMBOL: s += str_size((bson_t::string_t*)k); break;
      default: s += 1;
    }
  }
  return s;
}



int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cerr << "Need a file\n";
    return 1;
  }

  std::ifstream file(argv[1], std::ifstream::binary);
  kaitai::kstream ks(&file);
  uint64_t sum = 0;
  bson_t doc(&ks);
  std::cout << doc_size(&doc);
  return 0;
}
