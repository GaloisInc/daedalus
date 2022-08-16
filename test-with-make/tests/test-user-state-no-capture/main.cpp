#include <iostream>
#include <chrono>
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

  bool timed = false;

  if (argc > 3) {
    cout << "Usage: " << argv[0] << " [--timed] [FILE]" << endl;
    return 1;
  }

  int fileArg = 0;
  if (argc == 3) {
    timed = true;
    fileArg = 2;
  } else {
    if (argc == 2) {
      if (strcmp("--timed",argv[1]) == 0) {
        timed = true;
      } else {
        fileArg = 1;
      }
    }
  }

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

  DDL::ParseError err;
  std::vector<DDL::ResultOf::parseMain> out;
  auto start = std::chrono::high_resolution_clock::now();
  int state = 0;
  parseMain(state,err,out,i);
  auto end  = std::chrono::high_resolution_clock::now();
  auto diff = std::chrono::duration_cast<std::chrono::milliseconds>(end - start)
            .count();
  double mb = double(i.length().rep()) / double(1024 * 1024);
  double secs = double(diff) / double(1000);
  double mb_s = (double)mb / secs;

  size_t resultNum = out.size();

  if (timed) {
    cout << "{ \"resultNum\": " << resultNum << endl;
    cout << ", \"input_mb\": " << mb << endl;
    cout << ", \"time_secs\": " << secs << endl;
    cout << ", \"mb_s\": " << mb_s << endl;
    cout << ", \"results\": " << endl;
  }

  if (resultNum == 0) {
    cout << err;
#if 0
    cout << "{ \"error\": \"\", \"offset\": " << err.offset;
    cout << ", \"stack\":\n  [ ";
    auto first = true;
    for ( auto && x : err.debugs ) {
      if (first) { first = false; } else cout << "  , ";
      cout << "{ \"current\": \"" << x.get_cur() << "\"";
      if (!x.get_history().empty()) {
        cout << ", \"history\": [";
        auto first_history = true;
        for ( auto &&h : x.get_history() ) {
          if (first_history) { first_history = false; } else cout << ", ";
          cout << "[ \"" << h.first << "\", " << h.second << "]";
        }
      }
      cout << "}\n";
    }
    cout << "  ]\n}" << endl;
#endif
    return 1;
  }

  for (size_t i = 0; i < resultNum; ++i) {
    cout << (i > 0 ? ", " : "[ ");
    DDL::toJS(cout,(DDL::ResultOf::parseMain)out[i]);
    if constexpr (DDL::hasRefs<DDL::ResultOf::parseMain>()) out[i].free();
  }
  cout << "]";

  if (timed) cout << "}";

  cout << endl;

  return 0;
}
