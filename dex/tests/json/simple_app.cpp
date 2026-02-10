#include <iostream>
#include <vector>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>

#include "parser/exporter.h"

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
  DDL::ParseError<DDL::Input> error;
  std::vector<User::JSON_value> results;
  auto input = argc > 1? inputFromFile(argv[1]) : DDL::Input("(test)", "[1,2,3]");
  parseJSON_value_strict(error, results, input);
  
  for(auto i : results) {
    auto x = json::exportJSON(i);
    std::cout << std::setw(2) << x << "\n";
  }
  if (results.size() == 0) {
    std::cerr << "parse error\n";
    std::cerr << error << "\n";
  }
  
  return 0;
}
