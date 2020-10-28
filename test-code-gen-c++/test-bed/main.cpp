#include <iostream>
#include "main_parser.h"

using namespace std;

int main() {
  DDL::Input i("test_input","aa");
  DDL::Parser<ParserResult> p(i);
  parser(p);

  auto v = p.getResults();
  size_t resultNum = v.size();

  cout << resultNum << " results:" << endl;

  if (resultNum == 0) {
    cout << "Parser error at " << p.getFailOffset() << endl;
    return 1;
  }

  for (size_t i = 0; i < resultNum; ++i) {
    // cout << v[i] << endl;
  }

  return 0;
}
