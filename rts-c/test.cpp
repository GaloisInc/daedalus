#include <string.h>
#include "parser.h"
#include "unit.h"

void f (Unit x) {}

uint8_t test(Parser<uint8_t> &p) {
  f(Unit());

  // const char *const bytes = "Hello";
  // Input x("Test", bytes, strlen(bytes));
  // Parser p(x);
  Input y(p.getInput());
  y.iDrop(1);
  p.setInput(y);
  return p.getInput().iHead();
}
