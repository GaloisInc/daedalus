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
#include <ddl/owned.h>
#include <main_parser.h>

#include "args.hpp"
#include "debug.hpp"
#include "state.hpp"
#include "catalog.hpp"
#include "primitives.hpp"

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

template<class Facet>
struct deletable_facet : Facet
{
    template<class ...Args>
    deletable_facet(Args&& ...args) : Facet(std::forward<Args>(args)...) {}
    ~deletable_facet() {}
};

int main(int argc, char* argv[]) {

  auto args = parse_args(argc, argv);

  DDL::Input input;
  if (!inputFromFile(args.inputFile.c_str(), &input)) {
    std::cerr << "Unable to open file" << std::endl;
    return 1;
  }

  bool text = args.extractText;

  bool reject = false;
  bool safe   = true;

  try {
    references.process_pdf(input);
    check_catalog(text);
    if (text) {
      try {

        std::cout << "PRINT TEXT HERE\n";
#if 0
        std::wstring_convert<deletable_facet<std::codecvt<char32_t, char, std::mbstate_t>>, char32_t> convert;
        auto u8bytes = convert.to_bytes(emittedCodepoints); 
        
        if (args.outputFile.empty()) {
          std::cout << u8bytes;
        } else {
          std::ofstream fout(args.outputFile);
          fout << u8bytes;
        }
#endif
      } catch (std::exception const& e) {
        std::cerr << e.what() << std::endl;
      }
      return 0;
    }

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
