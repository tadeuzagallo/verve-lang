#include "generator.h"

namespace ceos {
  bool Generator::generate(void) const {
    generateProgram(m_ast);
    return true;
  }

  void Generator::generateNode(std::shared_ptr<AST> node) const {
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

  bool Generator::handleSpecialCall(std::shared_ptr<AST::Call> call) const {
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

  void Generator::generateCall(std::shared_ptr<AST::Call> call) const {
    if (handleSpecialCall(call)) {
      return;
    }

    for (unsigned i = call->arguments.size(); i > 0;) {
      generateNode(call->arguments[--i]);
    }

    m_output << "push $" << call->arguments.size() << "\n";
    m_output << "call\n";
  }

  void Generator::generateNumber(std::shared_ptr<AST::Number> number) const {
    m_output << "push $" << number->value << "\n";
  }

  void Generator::generateID(std::shared_ptr<AST::ID> id) const {
    m_output << "push $" << id->name << "\n";
  }

  void Generator::generateFunction(std::shared_ptr<AST::Call> fn) const {
    m_output << "fn " << AST::asID(fn->arguments[1])->name;
    m_output << "(" << AST::asCall(fn->arguments[2])->arguments.size() << "):\n";

    for (auto arg : AST::asCall(fn->arguments[2])->arguments) {
      m_output << "pop $" << AST::asID(arg)->name << "\n";
    }
    
    generateNode(fn->arguments[3]);
  }

  void Generator::generateIf(std::shared_ptr<AST::Call> iff) const {
    unsigned size = iff->arguments.size();
    assert(size == 3 || size == 4);

    generateNode(iff->arguments[1]);

    m_output << "if " << 9 << "\n";

    generateNode(iff->arguments[2]);

    if (size == 4) {
      m_output << "jump " << 7 << "\n";
      generateNode(iff->arguments[3]);
    }
  }
  
  void Generator::generateProgram(std::shared_ptr<AST::Program> program) const {
    for (auto node : program->nodes()) {
      generateNode(node);
    }

    if (program->functions.size()) {
      m_output << "FUNCTIONS:\n";
      for (auto fn : program->functions) {
        generateFunction(fn);
      }
    }
  }
}
