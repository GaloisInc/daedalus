#include <iostream>
#include <sstream>
#include <fstream>

#include <ddl/input.h>
#include <ddl/number.h>
#include <ddl/unit.h>
#include <ddl/utils.h>

#include "build/ddl/example2/main_parser.h"

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

// For simplicty we keep these as globals.
// The "proper" way to do this would be to use custom user defined state.
// See `example3` for how to do that.
struct CustomState {
  char  **files;
  size_t file_number;
  size_t next_file;
} global_state;


// This is the implementation of the `GetPacketBytes` primitive.
bool parser_GetPacketBytes
  ( DDL::ParserState<DDL::Input>& state
  , DDL::Unit *result
  , DDL::Input *newInput
  , DDL::Input currentInput
) {

  std::cout << "Getting next packet\n";

  // Since we don't need the currentInput we dellocate it
  currentInput.free();

  // Parser fails if we don't have any more "packets"
  if (global_state.next_file >= global_state.file_number) return false;

  char *name = global_state.files[global_state.next_file];
  // Load packet bytes from a file
  if (!inputFromFile(name, newInput)) {
    std::cerr << "Failed to open file " << name << std::endl;
    return false; // We also fail if we failed to open the file
  }

  ++global_state.next_file;   // Get ready for next packet
  *result = DDL::Unit();      // No interesting result to return

  return true;                // Success!
}


int main(int argc, char *argv[]) {

  global_state.files       = argv;
  global_state.file_number = argc;
  global_state.next_file   = 1;

  // Now parse the given input with the Protocol parser
  DDL::ParseError<DDL::Input> error;    // Inforation about errors goes here
  DDL::Input empty;         // Some empty input
  User::Protocol result;    // Parsed result goes here
    if (!DDL::parseOne(parseProtocol, error, &result, empty)) {
      std::cerr << "Failed to parse protocol:" << std::endl;
      std::cerr << error;
      return 1;
    }

  std::cout << result << std::endl;
  result.free();

  return 0;
}

