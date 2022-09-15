#include <iostream>
#include <sstream>
#include <fstream>

#include <ddl/input.h>
#include <ddl/number.h>
#include <ddl/unit.h>
#include <ddl/utils.h>

#include "build/ddl/example3/main_parser.h"
#include "example3-driver.h"

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

// This is the implementation of the `GetPacketBytes` primitive.
bool parser_GetPacketBytes
  ( DDL::ParserStateUser<CustomState>& state
  , DDL::Unit *result
  , DDL::Input *newInput
  , DDL::Input currentInput
) {

  CustomState &user = state.getUserState();

  std::cout << "Getting next packet\n";

  // Since we don't need the currentInput we dellocate it
  currentInput.free();

  // Parser fails if we don't have any more "packets"
  if (user.next_file >= user.file_number) return false;

  char *name = user.files[user.next_file];
  // Load packet bytes from a file
  if (!inputFromFile(name, newInput)) {
    std::cerr << "Failed to open file " << name << std::endl;
    return false; // We also fail if we failed to open the file
  }

  ++user.next_file;   // Get ready for next packet
  *result = DDL::Unit();      // No interesting result to return

  return true;                // Success!
}


int main(int argc, char *argv[]) {

  CustomState user;
  user.files       = argv;
  user.file_number = argc;
  user.next_file   = 1;

  // Now parse the given input with the Protocol parser
  DDL::ParseError error;    // Inforation about errors goes here
  DDL::Input empty;         // Some empty input
  User::Protocol result;    // Parsed result goes here
    if (!DDL::parseOneUser(parseProtocol, user, error, &result, empty)) {
      std::cerr << "Failed to parse protocol:" << std::endl;
      std::cerr << error;
      return 1;
    }

  std::cout << result << std::endl;
  result.free();

  return 0;
}

