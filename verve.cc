#include <cassert>
#include <cstdio>
#include <fstream>
#include <libgen.h>

#include "parser/lexer.h"
#include "parser/parser.h"
#include "bytecode/generator.h"
#include "bytecode/disassembler.h"
#include "runtime/vm.h"

int main(int argc, char **argv) {
  ROOT_DIR = dirname(argv[0]);

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

  auto filename = isDebug || isCompile ? argv[2] : argv[1];

  FILE *source = fopen(filename, "r");
  fseek(source, 0, SEEK_END);
  size_t sourceSize = ftell(source);
  fseek(source, 0, SEEK_SET);

  char *input = (char *)malloc(sourceSize + 1);
  fread(input, 1, sourceSize, source);
  input[sourceSize] = '\0';

  fclose(source);

  Verve::Lexer lexer(filename, input);

  auto dir = dirname(filename);

  Verve::Parser parser(lexer, dir);

  std::shared_ptr<Verve::AST::Program> ast = parser.parse();

  free(input);

  Verve::Generator generator(ast, isDebug);

  std::stringstream &bytecode = generator.generate();

  if (isDebug) {
    Verve::Disassembler disassembler(std::move(bytecode));
    disassembler.dump();
  } else if (isCompile) {
    std::ofstream output(argv[3], std::ios_base::binary);
    output << bytecode.str();
  } else {
    Verve::VM vm(bytecode);
    vm.execute();
  }

  return EXIT_SUCCESS;
}
