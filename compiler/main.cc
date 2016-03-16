#include <cassert>
#include <cstdio>
#include <fstream>

#include "./lexer.h"
#include "./parser.h"
#include "./generator.h"

int main(int argc, char **argv) {
  char *first = argv[1];
  bool isDebug = strcmp(first, "-d") == 0;
  bool isCompile = strcmp(first, "-c") == 0;

  if (isCompile) {
    assert(argc == 4);
  } else if (isDebug) {
    assert(argc == 3);
  } else {
    assert(argc == 2);
  }

  std::ifstream input(isDebug || isCompile ? argv[2] : argv[1]);

  ceos::Lexer lexer(input);
  ceos::Parser parser(lexer);

  std::shared_ptr<ceos::AST::Program> ast = parser.parse();

  ceos::Generator generator(ast);

  std::stringstream &bytecode = generator.generate();

  if (isDebug) {
    ceos::Generator::disassemble(bytecode);
  } else if (isCompile) {
    std::ofstream output(argv[3], std::ios_base::binary);
    output << bytecode.str();
  } else {
    // TODO
  }

  return EXIT_SUCCESS;
}
