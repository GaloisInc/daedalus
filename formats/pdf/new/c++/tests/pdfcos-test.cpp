#include <ddl/input.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <pdfcos.hpp>

int main(int argc, char *argv[]) {

  if (argc < 2) {
    std::cerr << "USAGE: " << (argc < 1 ? "Need" : argv[0]) << " FILE\n";
    return 1;
  }

  std::ifstream fin {argv[1], std::ios::in | std::ios::binary};

  if (!fin.is_open()) {
    std::cerr << "Failed to open " << argv[1] << std::endl;
    return 1;
  }

  std::ostringstream sout;
  sout << fin.rdbuf();
  std::string str = std::move(sout.str());

  DDL::Input input{argv[1], str.data(), str.size()};

  ReferenceTable refs;

  try {
    refs.process_pdf(input);

    for (auto && [refid, val] : refs.table) {
      User::Ref ref;
      ref.init(DDL::Integer{refid}, DDL::Integer{val.gen});
      std::cout << ref << std::endl;
      ref.free();
    }

  } catch (XrefException const& e) {
    std::cerr << "ERROR: [XRef] " << e.what() << std::endl;
  }

  return 0;
}
