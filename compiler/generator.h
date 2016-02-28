#include <fstream>

#ifndef CEOS_GENERATOR_H
#define CEOS_GENERATOR_H

namespace ceos {

  class AST;

  class Generator {
    public:
      Generator(const AST &ast, const std::ofstream &output) :
        m_ast(ast),
        m_output(output) {}

      bool generate(void) const;

    private:
      const AST &m_ast;
      const std::ofstream &m_output;
  };

}

#endif
