#include "predictor.hpp"

#include <cstdlib>
#include <limits>

namespace {

bool checkedAdd(size_t x, size_t y, size_t &result) {
  if (y > std::numeric_limits<size_t>::max() - x) {
    return false;
  }
  result = x + y;
  return true;
}

bool checkedMultiply(size_t x, size_t y, size_t &result) {
  if (x != 0 && y > std::numeric_limits<size_t>::max() / x) {
    return false;
  }
  result = x * y;
  return true;
}

uint8_t paethPredictor(uint8_t left, uint8_t above, uint8_t upperLeft) {
  int const estimate = int(left) + int(above) - int(upperLeft);
  int const leftDistance = std::abs(estimate - int(left));
  int const aboveDistance = std::abs(estimate - int(above));
  int const upperLeftDistance = std::abs(estimate - int(upperLeft));

  if (leftDistance <= aboveDistance && leftDistance <= upperLeftDistance) {
    return left;
  }
  if (aboveDistance <= upperLeftDistance) {
    return above;
  }
  return upperLeft;
}

uint8_t pngPrediction(
  uint8_t tag,
  uint8_t left,
  uint8_t above,
  uint8_t upperLeft
) {
  switch (tag) {
    case 0: return 0;
    case 1: return left;
    case 2: return above;
    case 3: return uint8_t((uint16_t(left) + uint16_t(above)) / 2);
    case 4: return paethPredictor(left, above, upperLeft);
    default: return 0;
  }
}

bool pngPredictor(
  uint64_t colorsValue,
  uint64_t bitsPerComponentValue,
  uint64_t columnsValue,
  std::string &bytes
) {
  if (colorsValue == 0 || columnsValue == 0) {
    return false;
  }
  if (bitsPerComponentValue != 1 &&
      bitsPerComponentValue != 2 &&
      bitsPerComponentValue != 4 &&
      bitsPerComponentValue != 8 &&
      bitsPerComponentValue != 16) {
    return false;
  }
  if (colorsValue > std::numeric_limits<size_t>::max() ||
      bitsPerComponentValue > std::numeric_limits<size_t>::max() ||
      columnsValue > std::numeric_limits<size_t>::max()) {
    return false;
  }

  size_t const colors = size_t(colorsValue);
  size_t const bitsPerComponent = size_t(bitsPerComponentValue);
  size_t const columns = size_t(columnsValue);

  size_t bitsPerPixel;
  size_t roundedBitsPerPixel;
  size_t rowBits;
  size_t roundedRowBits;
  size_t rowSize;
  if (!checkedMultiply(colors, bitsPerComponent, bitsPerPixel) ||
      !checkedAdd(bitsPerPixel, 7, roundedBitsPerPixel) ||
      !checkedMultiply(bitsPerPixel, columns, rowBits) ||
      !checkedAdd(rowBits, 7, roundedRowBits)) {
    return false;
  }

  size_t const bytesPerPixel = roundedBitsPerPixel / 8;
  size_t const rowBytes = roundedRowBits / 8;
  if (!checkedAdd(rowBytes, 1, rowSize) || bytes.size() % rowSize != 0) {
    return false;
  }
  if (bytes.empty()) {
    return true;
  }

  auto *data = reinterpret_cast<uint8_t *>(bytes.data());

  uint8_t const firstTag = data[0];
  if (firstTag > 4) {
    return false;
  }
  for (size_t column = 0; column < rowBytes; column++) {
    uint8_t const byte = data[column + 1];
    uint8_t const left =
      column < bytesPerPixel ? 0 : data[column - bytesPerPixel];
    data[column] = byte + pngPrediction(firstTag, left, 0, 0);
  }

  size_t readRow = rowSize;
  size_t write = rowBytes;
  while (readRow < bytes.size()) {
    uint8_t const tag = data[readRow];
    if (tag > 4) {
      return false;
    }

    size_t const rowEnd = readRow + rowSize;
    for (size_t read = readRow + 1, column = 0;
         read < rowEnd;
         read++, column++) {
      uint8_t const byte = data[read];
      uint8_t const left =
        column < bytesPerPixel ? 0 : data[write + column - bytesPerPixel];
      uint8_t const above = data[write - rowBytes + column];
      uint8_t const upperLeft =
        column < bytesPerPixel
          ? 0
          : data[write - rowBytes + column - bytesPerPixel];
      data[write + column] =
        byte + pngPrediction(tag, left, above, upperLeft);
    }
    write += rowBytes;
    readRow = rowEnd;
  }

  bytes.resize(write);
  return true;
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
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
      return pngPredictor(colors, bpc, columns, bytes);
    default: return false; // unsupported
  }
}
