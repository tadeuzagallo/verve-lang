#include <fstream>

#include "ast.h"

#ifndef CEOS_GENERATOR_H
#define CEOS_GENERATOR_H

namespace ceos {

  class Generator {
    public:
      Generator(const AST::Program &ast, std::ofstream &output) :
        m_ast(ast),
        m_output(output) {}

      bool generate(void) const;

    private:
      const AST::Program &m_ast;
      std::ofstream &m_output;
  };

}

#endif
