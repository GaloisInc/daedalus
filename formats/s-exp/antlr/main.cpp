#include <cstdint>
#include <iostream>
#include <any>
#include "antlr4-runtime.h"
#include "sexprVisitor.h"
#include "sexprBaseVisitor.h"
#include "sexprLexer.h"
#include "sexprParser.h"

struct Count : public sexprBaseVisitor {
  uint64_t count = 0;
  antlrcpp::Any visitSymbol(sexprParser::SymbolContext *context) override {
    ++count;
    return {};
  }
};


int main() {
  antlr4::ANTLRFileStream stream("../data.dat");
  auto lexer  = sexprLexer(&stream);
  auto tokens = antlr4::CommonTokenStream(&lexer);
  auto parser = sexprParser(&tokens);
  auto error_handler = std::make_shared<antlr4::BailErrorStrategy>();
  parser.setErrorHandler(error_handler);
  try {
    auto *ast   = parser.sexp();
    auto count  = Count{};
    ast->accept(&count);
    std::cout << count.count << std::endl;
  } catch (antlr4::ParseCancellationException const& ex) {
    std::cout << "parse error" << std::endl;
  }
  return 0;
}
