#include <iostream>
#include <chrono>
#include <unordered_set>
#include <fstream>
#include <sstream>
#include <exception>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>

#include <ddl/input.h>
#include <ddl/number.h>
#include <main_parser.h>

#include "debug.hpp"
#include "state.hpp"
#include "catalog.hpp"
#include "Owned.hpp"

bool inputFromFile(const char *file, DDL::Input *input)
{
  std::ifstream fin {file, std::ios::in | std::ios::binary};
  std::ostringstream sout;

  if (!fin.is_open()) {
    return false;
  }

  sout << fin.rdbuf();  
  std::string str = std::move(sout.str());

  *input = DDL::Input{file, str.data(), str.size()};
  return true;
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " FILE\n";
    return 1;
  }

  DDL::Input input;
  if (!inputFromFile(argv[1], &input)) {
    std::cerr << "unable to open file" << std::endl;
    return 1;
  }

  try {
    references.process_pdf(input);
  } catch (XrefException const& e) {
    std::cerr << "Error while processing cross-references: " << e.what() << std::endl;
    return 1;
  }

  check_catalog();

  for (auto && [refid, val] : references.table) {
    dbg << "Getting " << std::dec << refid << " " << val.gen << std::endl;
    DDL::Maybe<User::TopDecl> decl;

    if (references.resolve_reference(refid, val.gen, &decl)) {
      decl.free();
    } else {
      dbg << "Failed\n";
    }
  }

  return 0;
}
