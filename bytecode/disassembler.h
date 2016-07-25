#include <iostream>
#include <sstream>
#include <vector>

#include "opcodes.h"
#include "sections.h"

namespace Verve {

class Disassembler {
public:
  Disassembler(std::stringstream &bytecode);
  void dump();

private:
  struct HelperStream {
    template <typename T>
    HelperStream &operator <<(T t) {
      std::cout << t;
      return *this;
    }

    ~HelperStream() {
      std::cout << std::endl;
    }
  };

  HelperStream write(double offset);
  int64_t read();
  std::string readStr();
  int calculateJmpTarget(int target);
  void printOpcode(Opcode::Type opcode);
  void dumpStrings();
  void dumpFunctions();
  void dumpText();

  std::stringstream &m_bytecode;
  std::vector<std::string> m_strings;
  std::vector<std::string> m_functions;
  size_t m_width;
  std::string m_padding = "  ";
};

}
