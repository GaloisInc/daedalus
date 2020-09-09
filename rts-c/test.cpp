#include <string.h>
#include "parser.h"
#include "unit.h"
#include "number.h"

void f (Unit x) {}

uint8_t test(Parser<uint8_t> p) {
  return p.getInput().iDrop(2).iHead();
}

int main() {
  SInt<7> x(127);
  std::cout << sizeof(size_t) << std::endl;
}
