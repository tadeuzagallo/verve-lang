#include <fstream>

#ifndef CEOS_LEXER_H
#define CEOS_LEXER_H

namespace ceos {

  class Lexer {
    public:
      Lexer(const std::ifstream &input) : m_input(input) {}

    private:
      const std::ifstream &m_input;
  };

}

#endif
