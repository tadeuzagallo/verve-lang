#include <cassert>
#include <cstdio>
#include <fstream>

#include "./lexer.h"
#include "./parser.h"
#include "./generator.h"
#include "./vm.h"

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

  FILE *prelude = fopen("builtins.ceos", "r");
  FILE *source = fopen(isDebug || isCompile ? argv[2] : argv[1], "r");
  fseek(prelude, 0, SEEK_END);
  fseek(source, 0, SEEK_END);
  size_t preludeSize = ftell(prelude);
  size_t sourceSize = ftell(source);
  fseek(prelude, 0, SEEK_SET);
  fseek(source, 0, SEEK_SET);

  char *input = (char *)malloc(preludeSize + sourceSize + 2);
  fread(input, 1, preludeSize, prelude);
  input[preludeSize] = '\n';
  fread(input + preludeSize + 1, 1, sourceSize, source);
  input[preludeSize + sourceSize + 1] = '\0';

  fclose(prelude);
  fclose(source);

  ceos::Lexer lexer(input, preludeSize);
  ceos::Parser parser(lexer);

  std::shared_ptr<ceos::AST::Program> ast = parser.parse();

  free(input);

  ceos::Generator generator(ast);

  std::stringstream &bytecode = generator.generate();

  if (isDebug) {
    ceos::Generator::disassemble(bytecode);
  } else if (isCompile) {
    std::ofstream output(argv[3], std::ios_base::binary);
    output << bytecode.str();
  } else {
    ceos::VM vm(bytecode);
    vm.execute();
  }

  return EXIT_SUCCESS;
}
