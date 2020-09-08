#include <string.h>
#include "parser.h"
#include "unit.h"

void f (Unit x) {}

uint8_t test(Parser<uint8_t> p) {
  return p.getInput().iDrop(2).iHead();
}

int main() {
  std::cout << test(Parser<uint8_t> { Input { "Hello", "World" } });
  std::cout << std::endl;
}
