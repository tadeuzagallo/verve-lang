#include <cassert>
#include <cstdio>
#include <fstream>

#include "./lexer.h"
#include "./parser.h"
#include "./generator.h"

int main(int argc, char **argv) {
  assert(argc == 3);

  const std::ifstream input(argv[1]);
  const std::ofstream output(argv[2]);

  const ceos::Lexer lexer(input);
  const ceos::Parser parser(lexer);
  const ceos::AST &ast = parser.parse();
  const ceos::Generator generator(ast, output);

  if (generator.generate()) {
    return EXIT_SUCCESS;
  } else {
    return EXIT_FAILURE;
  }
}
