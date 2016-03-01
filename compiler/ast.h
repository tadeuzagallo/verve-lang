#include <memory>
#include <vector>
#include <string>

#ifndef CEOS_AST_H
#define CEOS_AST_H

namespace ceos {
  
  class AST {
    public:
      class Program;
      class Call;

  };

  class AST::Program : public AST {
    public:
      void addNode(std::shared_ptr<AST> node) {
        m_nodes.push_back(node);
      }

      std::vector<std::shared_ptr<AST>> nodes() const { return m_nodes; }

    private:
      std::vector<std::shared_ptr<AST>> m_nodes;
  };

  class AST::Call : public AST {
    public:
      Call(std::string c) : callee(c) {}

      std::string callee;
      std::vector<std::shared_ptr<AST>> params;
  };

}

#endif
