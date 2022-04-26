#include "predictor.hpp"

namespace {

void pngUp(size_t columns, std::string &bytes) {
    size_t const row_size = 1 + columns;

    std::string output;
    output.reserve(bytes.size() / row_size * columns);

    // Every row starts with a predictor tag, so all the code
    // in this function starts processing at index 1 and proceeds
    // to index (columns)

    size_t const n = bytes.size();

    for (size_t i = 1; i < row_size && i < n; i++) {
      output.push_back(bytes[i]);
    }

    for (size_t cursor = 0, i = row_size; i < n; i++) {
      if (i % row_size) {
        output.push_back(output[cursor++] + bytes[i]);
      }
    }

    bytes = std::move(output);
}

}

bool unpredict(
  uint64_t predictor,
  uint64_t colors,
  uint64_t bpc,
  uint64_t columns,
  std::string &bytes
) {
  switch (predictor) {
    case 1: return true; // no predictor
    case 12: pngUp(columns, bytes); return true;
    default: return false; // unsupported
  }
}
