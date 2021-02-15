#include <iostream>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include "main_parser.h"

using namespace std;


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

  if (argc > 2) {
    cout << "Usage: " << argv[0] << " [FILE]" << endl;
    return 1;
  }

  DDL::Input i;
  if (argc == 1) {
    i = DDL::Input("(none)","");
  } else {
    char *file = argv[1];
    i = inputFromFile(file);
    if (i.length() == 0) {
      // Does not escape quotes...
      cout << "Failed to open file \"" << file << '"' << endl;
      return 1;
    }
  }

  DDL::ParseError err;
  std::vector<DDL::ResultOf::parseMain> out;
  parseMain(i,err,out);

  size_t resultNum = out.size();

  cout << "{ \"resultNum\": " << resultNum << endl;
  cout << ", \"results\": " << endl;

  if (resultNum == 0) {
    cout << "\"Parser error at " << err.offset << "\"" << endl;
    return 1;
  }

  for (size_t i = 0; i < resultNum; ++i) {
    cout << (i > 0 ? ", " : "[ ");
    DDL::toJS(cout,(DDL::ResultOf::parseMain)out[i]);
  }
  cout << "]}\n";

  return 0;
}
