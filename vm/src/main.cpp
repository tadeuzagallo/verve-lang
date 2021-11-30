#include "Bytecode.h"
#include "Decoder.h"
#include "VM.h"
#include "Interpreter.h"

#include <cassert>
#include <iostream>
#include <fstream>
#include <vector>

int main(int argc, char **argv)
{
  assert(argc == 2);
  std::ifstream is(argv[1], std::ifstream::binary);

  Verve::Decoder decoder(std::move(is));
  Verve::VM vm(decoder.decode());
  Verve::Interpreter interpreter(vm);
  interpreter.run();

  return 0;
}
