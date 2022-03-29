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
    std::cerr << "Unable to open file" << std::endl;
    return 1;
  }

  bool text = false;

  bool reject = false;
  bool safe   = true;

  try {
    references.process_pdf(input);
    check_catalog(text);
    if (text) return 0;

    for (auto && [refid, val] : references.table) {
      User::Ref ref;
      ref.init(DDL::Integer{refid}, DDL::Integer{val.gen});
      std::vector<DDL::Bool> results;
      DDL::ParseError error;
      parseCheckRef(error, results, DDL::Input{"",""}, ref);
      if (results.size() != 1) {
        std::cerr << "ERROR: [" << refid << "," << val.gen << "] Malformed\n";
        reject = true;
      } else {
        safe &= results[0].getValue();
        if (!results[0].getValue())
          std::cerr << "INFO: [" << refid << "," << val.gen << "] is unsafe\n";
      }
    }

  } catch (XrefException const& e) {
    std::cerr << "ERROR: [XRef] " << e.what() << std::endl;
    reject = true;

  } catch (CatalogException const& e) {
    std::cerr << "ERROR: [Catalog] " << e.what() << std::endl;
    reject = true;
  }

  std::cerr << (reject?          "REJECT" : "ACCEPT") << std::endl;
  std::cerr << (!reject && safe? "SAFE"   : "UNSAFE") << std::endl;

  return 0;
}
