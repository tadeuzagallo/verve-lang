#include "file.h"

#include "parser/lexer.h"
#include "parser/parser.h"

#include <cassert>
#include <cstdio>
#include <fstream>

namespace Verve {

Parser parseFile(std::string filename, std::string dirname, std::string ns) {
  filename = dirname + "/" + filename + ".vrv";

  FILE *source = fopen(filename.c_str(), "r");
  fseek(source, 0, SEEK_END);
  size_t sourceSize = ftell(source);
  fseek(source, 0, SEEK_SET);

  char *input = (char *)malloc(sourceSize + 1);
  fread(input, 1, sourceSize, source);
  input[sourceSize] = '\0';

  fclose(source);

  Lexer lexer(input, 0);
  Parser parser(lexer, dirname, ns);

  parser.parse();

  return parser;
}

}
