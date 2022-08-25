#include <ddl/input.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <ddl/utils.h>
#include <pdfcos.hpp>
#include <pdf-validate.h>

int main(int argc, char *argv[]) {

  // --- Setup Input -----------------------------------
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
  // -------------------------------------------------



  ReferenceTable refs;

  input.copy();
  try {
    refs.process_pdf(input);

    for (auto && [refid, val] : refs.table) {
      PdfCos::Ref ref;
      ref.init(DDL::Integer{refid}, DDL::Integer{val.gen});
      std::cout << ref << std::endl;
      DDL::ParseError err;
      PdfCos::Value out;
      input.copy();
      if (parseOneUser(parseValidate,refs,err,&out,input,ref)) {
        std::cout << out;
        out.free();
      } else {
        std::cout << err << std::endl;
      }
    }

  } catch (XrefException const& e) {
    std::cerr << "ERROR: [XRef] " << e.what() << std::endl;
  }
  input.free();

  return 0;
}
