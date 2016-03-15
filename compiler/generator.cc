#include "opcodes.h"
#include "generator.h"

#define FUNCTION_MAGIC_NUMBER 0xF00C

namespace ceos {
  bool Generator::generate(bool isDisassembly) {
    generateProgram(m_ast);
    if (isDisassembly) {
      disassemble();
    } else {
      m_fileOutput << m_output.str();
    }
    return true;
  }

  void Generator::generateNode(std::shared_ptr<AST> node) {
    switch (node->type) {
      case AST::Type::Call:
        generateCall(std::static_pointer_cast<AST::Call>(node));
        break;
      case AST::Type::Number:
        generateNumber(std::static_pointer_cast<AST::Number>(node));
        break;
      case AST::Type::ID:
        generateID(std::static_pointer_cast<AST::ID>(node));
        break;
      default:
        throw "Unhandled Type";
    }
  }

  bool Generator::handleSpecialCall(std::shared_ptr<AST::Call> call) {
    if (call->arguments[0]->type == AST::Type::ID) {
      std::string callee = AST::asID(call->arguments[0])->name;

      if (callee == "defn") {
        m_ast->functions.push_back(call);
        return true;
      } else if (callee == "if") {
        generateIf(call);
        return true;
      }
    }

    return false;
  }

  void Generator::write(int data) {
    m_output.write(reinterpret_cast<char *>(&data), sizeof(data));
  }

  void Generator::write(const std::string &data) {
    m_output << data;
  }

  void Generator::emitOpcode(Opcode::Type opcode) {
    write(opcode);
  }

  void Generator::generateCall(std::shared_ptr<AST::Call> call) {
    if (handleSpecialCall(call)) {
      return;
    }

    for (unsigned i = call->arguments.size(); i > 0;) {
      generateNode(call->arguments[--i]);
    }

    emitOpcode(Opcode::push);
    write(call->arguments.size());
    emitOpcode(Opcode::call);
  }

  void Generator::generateNumber(std::shared_ptr<AST::Number> number) {
    emitOpcode(Opcode::push);
    write(number->value);
  }

  void Generator::generateID(std::shared_ptr<AST::ID> id) {
    emitOpcode(Opcode::load_string);
    write(id->uid);
    emitOpcode(Opcode::lookup);
  }

  void Generator::generateFunction(std::shared_ptr<AST::Call> fn) {
    m_output << "fn " << AST::asID(fn->arguments[1])->name;
    m_output << "(" << AST::asCall(fn->arguments[2])->arguments.size() << "):\n";

    for (auto arg : AST::asCall(fn->arguments[2])->arguments) {
      emitOpcode(Opcode::pop);
      write(AST::asID(arg)->name);
    }
    
    generateNode(fn->arguments[3]);
  }

  void Generator::generateIf(std::shared_ptr<AST::Call> iff) {
    unsigned size = iff->arguments.size();
    assert(size == 3 || size == 4);

    generateNode(iff->arguments[1]);

    emitOpcode(Opcode::jz);
    write(9);

    generateNode(iff->arguments[2]);

    if (size == 4) {
      emitOpcode(Opcode::jmp);
      write(7);
      generateNode(iff->arguments[3]);
    }
  }
  
  void Generator::generateProgram(std::shared_ptr<AST::Program> program) {
    for (auto node : program->nodes()) {
      generateNode(node);
    }

    if (program->functions.size()) {
      write(FUNCTION_MAGIC_NUMBER);
      for (auto fn : program->functions) {
        //generateFunction(fn);
      }
    }
  }

  void Generator::disassemble() {
#define READ_INT(INT_NAME) \
      int INT_NAME; \
      m_output.get(reinterpret_cast<char *>(&INT_NAME), 5);

#define WRITE(...) m_fileOutput << __VA_ARGS__ << "\n"

    m_output.seekg(0);

    while (!m_output.eof()) {
      READ_INT(opcode);

      switch (opcode) {
        case Opcode::push: {
          READ_INT(value);
          WRITE("push $" << value);
          break;
        }
        case Opcode::call: {
          WRITE("call");
          break;
        }
        case Opcode::load_string: {
          READ_INT(stringID);
          WRITE("load_string $" << stringID);
          break;
        }
        case Opcode::lookup: {
          WRITE("lookup");
          break;
        }
        case Opcode::jmp:  {
          READ_INT(target);
          WRITE("jmp " << target);
          break;
        }
        case Opcode::jz: {
          READ_INT(target);
          WRITE("jz " << target);
          break;
        }
        case FUNCTION_MAGIC_NUMBER: {
          WRITE("FUNCTIONS:");
          break;
        }
        default:
          break;
      }

    }

#undef READ_INT
#undef WRITE
  }
}
