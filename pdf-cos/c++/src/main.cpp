#include <iostream>
#include <chrono>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>

#include <ddl/input.h>
#include <ddl/number.h>
#include <main_parser.h>

const size_t not_found = (size_t) (-1);

size_t findPdfEnd(size_t len, const char *bytes) {
  const char *tgt = "startxref";
  const size_t tgt_last = 8;

  if (len <= tgt_last) return not_found;

  // Look for startxref
  size_t ix = len - 1;
  size_t tgt_ix = tgt_last;
  while (true) {
    if (bytes[ix] == tgt[tgt_ix]) {
      if (tgt_ix == 0) return ix;
      if (ix == 0) return not_found;
      --tgt_ix;
      --ix;
    } else {
      if (ix == 0) return not_found;
      if (tgt_ix == tgt_last) --ix; else tgt_ix = tgt_last;
    }
  }
}

DDL::Input inputFromFile(const char *file) {

  DDL::Input result{};
  char *bytes;
  size_t size;

  int fd = open(file,O_RDONLY);
  if (fd == -1) return result;

  struct stat info;
  if (fstat(fd, &info) != 0) goto done;

  size = info.st_size;
  bytes = (char*)mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (bytes == MAP_FAILED) goto done;

  result = DDL::Input{file,bytes,size};

  munmap (bytes, size); // wecopied them

done:
  close(fd);
  return result;
}




int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " FILE\n";
    return 1;
  }

  auto input = inputFromFile(argv[1]);
  auto end   = findPdfEnd(input.length(), input.borrowBytes());
  if (end == not_found) {
    std::cerr << "`startxref` not found\n";
    input.free();
    return 1;
  }

  std::cout << "PDF found at offset: " << end << std::endl;

  DDL::ParseError error;
  std::vector<DDL::UInt<64>> results;

  input.copy();
  DDL::Input x = input.iDrop(DDL::UInt<64>(end));
  x.dumpInput();
  parsePdfEnd(x, error, results);

  // XXX: It looks like the parser is not consuming `x` as expected.

  switch (results.size()) {
    case 0:
      std::cerr << "Parse error at offset " << error.offset << std::endl;
      std::cerr << "Failed to parse xref offset\n";
      input.free();
      return 2;

    case 1:
      break;

    default:
      std::cerr << "[bug] Ambiguous xref offset\n";
      input.free();
      return 3;
  }

  std::cout << "XREF offset = " << results[0] << std::endl;

  input.free();
  return 0;
}
