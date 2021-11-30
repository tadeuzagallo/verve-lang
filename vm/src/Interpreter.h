#pragma once

#include "Opcodes.h"
#include "VM.h"

namespace Verve {

class VM;

class Interpreter {
public:
  Interpreter(VM&);

  void run();

private:
  VM& m_vm;
};

}
