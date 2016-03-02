#include <memory>
#include <vector>
#include <string>

#ifndef CEOS_AST_H
#define CEOS_AST_H

namespace ceos {
  
  class AST {
    public:
      enum class Type {
        Program = 1,
        Call,
        Number,
        ID,
      };

      class Program;
      class Call;
      class Number;
      class ID;

      Type type;

      AST(Type t) : type(t) {}

  };

  class AST::Program : public AST {
    public:
      Program() : AST(Type::Program) {}

      void addNode(std::shared_ptr<AST> node) {
        m_nodes.push_back(node);
      }

      std::vector<std::shared_ptr<AST>> nodes() const { return m_nodes; }

    private:
      std::vector<std::shared_ptr<AST>> m_nodes;
  };

  class AST::Number : public AST {
    public:
      Number(int v) : AST(Type::Number), value(v) {}

      int value;
  };

  class AST::ID : public AST {
    public:
      ID(std::string n) : AST(Type::ID), name(n) {}

      std::string name;
  };

  class AST::Call : public AST {
    public:
      Call() : AST(Type::Call) {}

      std::vector<std::shared_ptr<AST>> arguments;
  };

}

#endif
