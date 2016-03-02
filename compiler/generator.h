#include <fstream>

#include "ast.h"

#ifndef CEOS_GENERATOR_H
#define CEOS_GENERATOR_H

namespace ceos {

  class Generator {
    public:
      Generator(std::shared_ptr<AST::Program> ast, std::ofstream &output) :
        m_ast(ast),
        m_output(output) {}

      bool generate(void) const;

    private:
      void generateNode(std::shared_ptr<AST>) const;
      void generateCall(std::shared_ptr<AST::Call>) const;
      void generateNumber(std::shared_ptr<AST::Number>) const;
      void generateID(std::shared_ptr<AST::ID>) const;
      void generateProgram(std::shared_ptr<AST::Program>) const;

      std::shared_ptr<AST::Program> m_ast;
      std::ofstream &m_output;
  };

}

#endif
