#include <iostream>
#include <chrono>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>

#include <ddl/float.h>
#include <ddl/json.h>
#include <ddl/input.h>

#include "state.h"
#include "parser/format.h"


// Make an input for the parser to process
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

done:
  close(fd);
  return result;
}

int main(int argc, char* argv[]) {

  // We need a parameter for the file to process
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " FILE\n";
    return 1;
  }

  auto input = inputFromFile(argv[1]);
  DDL::ParseError<DDL::Input> err;
  std::vector<DDL::ResultOf::parsePackets> results;

  State state('A');   // Initial application specific state
  parsePackets(state,err,results,input); // Run the parser

  // If we got no result, then there was a parse error
  if (results.size() == 0) {
    std::cerr << "Parse error: " << err << std::endl;
    return 2;
  }

  // If we got some result we just print them and deallocate them
  for (auto result : results) {
    DDL::toJS(std::cout, result);
    std::cout << std::endl;
    result.free();
  }

  return 0;
}