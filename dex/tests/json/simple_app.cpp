
#include <vector>
#include "parser/exporter.h"

int main() {
  DDL::ParseError<DDL::Input> error;
  std::vector<User::JSON_value> results;
  parseJSON_value_strict(error, results, DDL::Input("(test)", "[1,2,3]"));
  
  for(auto i : results) {
    auto x = json::exportJSON(i);
    std::cout << x << "\n";
  }
  if (results.size() == 0) {
    std::cout << "parse error\n";
  }
  
  return 0;
}
