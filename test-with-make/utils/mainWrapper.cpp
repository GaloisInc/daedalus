#include <iostream>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include <ddl/input.h>

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

int go(DDL::Input);

int main(int argc, char* argv[]) {

  if (argc > 2) {
    cout << "Usage: " << argv[0] << " [FILE]" << endl;
    return 1;
  }

  int fileArg = 0;
    if (argc == 2) fileArg = 1;

  DDL::Input i;
  if (fileArg == 0) {
    i = DDL::Input("(none)","");
  } else {
    char *file = argv[fileArg];
    i = inputFromFile(file);
    if (i.length() == 0) {
      // Does not escape quotes...
      cout << "Failed to open file \"" << file << '"' << endl;
      return 1;
    }
  }

  return go(i);
}


