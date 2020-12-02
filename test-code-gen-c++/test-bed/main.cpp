#include <iostream>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include "main_parser.h"

using namespace std;


// Just a quick hack, doesn't close file descriptor
char *getBytes(const char *file, size_t &size) {

  int fd = open(file,O_RDONLY);
  if (fd == -1) return nullptr;

  struct stat info;
  if (fstat(fd, &info) != 0) return nullptr;

  size = info.st_size;
  char* bytes = (char*)mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (bytes == MAP_FAILED) return nullptr;

  return bytes;
}




int main(int argc, char* argv[]) {

  if (argc > 2) {
    cout << "Usage: " << argv[0] << " [FILE]" << endl;
    return 1;
  }

  DDL::Input i;
  if (argc == 1) {
    i = DDL::Input("(none)","");
  } else {
    char *file = argv[1];
    size_t size = 0;
    char *bytes = getBytes(file,size);
    if (bytes == nullptr) {
      // Does not escape quotes...
      cout << "Failed to open file \"" << file << '"' << endl;
      return 1;
    }
    i = DDL::Input(file,bytes,size);
  }

  DDL::Parser<ParserResult> p(i);
  parser(p);

  auto v = p.getResults();
  size_t resultNum = v.size();

  cout << resultNum << " results:" << endl;

  if (resultNum == 0) {
    cout << "Parser error at " << p.getFailOffset() << endl;
    return 1;
  }

  for (size_t i = 0; i < resultNum; ++i) {
    cout << v[i] << endl;
  }

  return 0;
}
