#include <iostream>
#include <chrono>
#include <unordered_set>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>

#include <ddl/input.h>
#include <ddl/number.h>
#include <main_parser.h>

#include "state.hpp"
#include "Owned.hpp"

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

  auto input = owned(inputFromFile(argv[1]));
  process_pdf(input.borrow());

  for (auto && [key, val] : references.table) {
    auto && [refid, gen] = key;
    std::cerr << "Getting " << refid << " " << gen << std::endl;
    DDL::Maybe<User::TopDecl> decl;

    if (references.resolve_reference(input.borrow(), refid, gen, &decl)) {
      DDL::toJS(std::cerr, decl);
      std::cerr << std::endl;
      decl.free();
    } else {
      std::cerr << "Failed\n";
    }
  }

  return 0;
}
