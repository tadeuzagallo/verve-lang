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

  void Generator::generateCall(std::shared_ptr<AST::Call> call) const {
    if (call->isDefn()) {
      m_ast->functions.push_back(call);
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
