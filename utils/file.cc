#include "file.h"

#include "parser/lexer.h"
#include "parser/parser.h"

#include <cassert>
#include <cstdio>
#include <fstream>

namespace ceos {

Parser parseFile(std::string filename, std::string dirname) {
  filename = dirname + "/" + filename + ".ceos";

  FILE *source = fopen(filename.c_str(), "r");
  fseek(source, 0, SEEK_END);
  size_t sourceSize = ftell(source);
  fseek(source, 0, SEEK_SET);

  char *input = (char *)malloc(sourceSize + 1);
  fread(input, 1, sourceSize, source);
  input[sourceSize + 1] = '\0';

  fclose(source);

  Lexer lexer(input, 0);
  Parser parser(lexer, dirname);

  parser.parse();

  return parser;
}

}
