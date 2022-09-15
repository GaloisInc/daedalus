#include <iostream>
#include <sstream>
#include <fstream>

#include <ddl/input.h>
#include <ddl/number.h>
#include <ddl/utils.h>

#include "build/ddl/example1/main_parser.h"

// Create a Deadlus input from a file
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


int main(int argc, char *argv[]) {

  for (size_t count = 1; count < argc; ++count) {
    std::cout << "Parsing packet " << count << std::endl;

    // Load packet bytes from a file
    DDL::Input input;
    if (!inputFromFile(argv[count], &input)) {
      std::cerr << "Failed to open file " << argv[count] << std::endl;
      return 1;
    }

    // Now parse the given input with the Packet parser
    DDL::ParseError error;    // Inforation about errors goes here
    User::Packet result;      // Parsed result goes here
    if (!DDL::parseOne(parsePacket, error, &result, input, DDL::UInt<64>(count))) {
      std::cerr << "Failed to parse packet:" << std::endl;
      std::cerr << error;
      return 2;
    }

    std::cout << result << std::endl;
    result.free();
  }

  return 0;
}

