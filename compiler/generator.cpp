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
      default:
        throw "Unhandled Type";
    }
  }

  void Generator::generateCall(std::shared_ptr<AST::Call> call) const {
    for (auto param : call->params) {
      generateNode(param);
    }

    m_output << "push $" << call->params.size() << "\n";
    m_output << "call " << call->callee << "\n";
  }
  
  void Generator::generateProgram(std::shared_ptr<AST::Program> program) const {
    for (auto node : program->nodes()) {
      generateNode(node);
    }
  }
}
