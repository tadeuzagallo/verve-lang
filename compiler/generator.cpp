#include "generator.h"

namespace ceos {
  bool Generator::generate(void) const {
    for (auto c : m_ast.nodes()) {
      auto call = std::static_pointer_cast<AST::Call>(c);
      for (auto p : call->params) {
        m_output << "push " << p;
      }
      m_output << "push $" << call->params.size() << "\n";
      m_output << "call " << call->callee << "\n";
    }
    return true;
  }
}
