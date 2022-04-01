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

void utf8(std::ostream &out, std::u32string str)
{
  for (uint32_t u : str) {
    if (u <= 0x7fU) {
      out << static_cast<unsigned char>(u);
    } else if (u <= 0x7ffU) {
      const unsigned char xs[2] {
        static_cast<unsigned char>(0xc0U | u>> 6 & 0x3fU),
        static_cast<unsigned char>(0x80U | u     & 0x3fU)};
      out.write(reinterpret_cast<char const*>(xs), 2);
    } else if (u <= 0xffffU) {
      const unsigned char xs[3] {
        static_cast<unsigned char>(0xe0U | u>>12 & 0x3fU),
        static_cast<unsigned char>(0x80U | u>> 6 & 0x3fU),
        static_cast<unsigned char>(0x80U | u     & 0x3fU)};
      out.write(reinterpret_cast<char const*>(xs), 3);
    } else if (u <= 0x10ffffU) {
      const unsigned char xs[4] {
        static_cast<unsigned char>(0xf0U | u>>18 & 0x3fU),
        static_cast<unsigned char>(0x80U | u>>12 & 0x3fU),
        static_cast<unsigned char>(0x80U | u>> 6 & 0x3fU),
        static_cast<unsigned char>(0x80U | u     & 0x3fU)};
      out.write(reinterpret_cast<char const*>(xs), 4);
    } else {
      const unsigned char xs[2] { 0xffU, 0xfdU };
      out.write(reinterpret_cast<char const*>(xs), 2);
    }
  }
}



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
      if (args.outputFile.empty()) {
        utf8(std::cout, emittedCodepoints);
      } else {
        std::ofstream fout(args.outputFile, std::ios::binary | std::ios::out);
        utf8(fout, emittedCodepoints);
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
