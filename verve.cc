#define _DARWIN_BETTER_REALPATH
#include <cassert>
#include <cstdio>
#include <fstream>
#include <libgen.h>
#include <mach-o/dyld.h>

#include "parser/lexer.h"
#include "parser/parser.h"
#include "bytecode/generator.h"
#include "bytecode/disassembler.h"
#include "runtime/vm.h"

void printUsage() {
  puts("Usage:");
  printf("  %-30s", "verve <input>");
  puts("Execute <input> as verve source code");

  printf("  %-30s", "verve -d <input>");
  puts("Print bytecode generated for <input>");

  printf("  %-30s", "verve -c <input> <output>");
  puts("Generate bytecode for <input> and save it at <output>");

  printf("  %-30s", "verve -b <input>");
  puts("Execute <input> as verve bytecode");
}

int main(int argc, char **argv) {
  char buffer[PATH_MAX];
  uint32_t bufferSize = PATH_MAX;
  _NSGetExecutablePath(buffer, &bufferSize);
  char buffer2[PATH_MAX];
  realpath(buffer, buffer2);
  ROOT_DIR = dirname(buffer2);

  char *first = argv[1];
  bool isDebug = first && strcmp(first, "-d") == 0;
  bool isCompile = first && strcmp(first, "-c") == 0;
  bool isBytecode = first && strcmp(first, "-b") == 0;
  bool isHelp = first && (strcmp(first, "-h") == 0 || strcmp(first, "--help") == 0);

  if (
      (isCompile && argc != 4) ||
      ((isDebug || isBytecode) && argc != 3) ||
      isHelp ||
      (!isHelp && !isDebug && !isCompile && !isBytecode && argc != 2)
     )
  {
    printUsage();
    return EXIT_FAILURE;
  }

  auto filename = isDebug || isCompile || isBytecode ? argv[2] : argv[1];

  FILE *source = fopen(filename, "r");

  if (!source) {
    char name[PATH_MAX];
    realpath(filename, name);
    printf("Error: Cannot open file at `%s`\n", name);
    return EXIT_FAILURE;
  }

  fseek(source, 0, SEEK_END);
  size_t sourceSize = ftell(source);
  fseek(source, 0, SEEK_SET);

  char *input = (char *)malloc(sourceSize + 1);
  fread(input, 1, sourceSize, source);
  input[sourceSize] = '\0';

  fclose(source);

  if (isBytecode) {
    Verve::VM vm((uint8_t *)input, sourceSize, true);
    vm.execute();
    free(input);
    return EXIT_SUCCESS;
  }

  auto dir = dirname(filename);

  Verve::Lexer lexer(filename, input);
  Verve::Parser parser(lexer, dir);
  std::shared_ptr<Verve::AST::Program> ast = parser.parse();

  Verve::Generator generator(ast, !isDebug && !isCompile);
  auto &bytecode = generator.generate();

  if (isDebug) {
    Verve::Disassembler disassembler(bytecode);
    disassembler.dump();
  } else if (isCompile) {
    std::ofstream output(argv[3], std::ios_base::binary);
    output << bytecode.str();
  } else {
    auto bc = bytecode.str();
    Verve::VM vm((uint8_t *)bc.data(), bc.size());
    vm.execute();
  }

  free(input);
  return EXIT_SUCCESS;
}
