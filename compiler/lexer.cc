#include "lexer.h"

#include "token.h"

#include <cassert>
#include <iomanip>
#include <iostream>
#include <sstream>

#define BASIC_TOKEN(CHAR, TYPE) \
  case CHAR: \
    m_token = std::make_shared<Token>(Token::Type::TYPE); \
    break;

namespace ceos {

  std::shared_ptr<Token> Lexer::nextToken() {
    char c;

    do {
      c = m_input.get();
    } while(isspace(c));

    int start = (int)m_input.tellg() - 1;
    switch (c) {
      BASIC_TOKEN('(', L_PAREN)
      BASIC_TOKEN(')', R_PAREN)
      BASIC_TOKEN('{', L_BRACE)
      BASIC_TOKEN('}', R_BRACE)
      BASIC_TOKEN(',', COMMA)

      case ':':
        assert(m_input.get() == ':');
        m_token = std::make_shared<Token>(Token::Type::TYPE);
        break;

      case '-':
        assert(m_input.get() == '>');
        m_token = std::make_shared<Token>(Token::Type::ARROW);
        break;

      case EOF:
        start = m_token != nullptr ? m_token->loc.end - 1 : 0;
        m_token = std::make_shared<Token>(Token::Type::END);
        break;

      case '#':
        do {
          c = m_input.get();
        } while (c != '\n');
        return nextToken();

      case '"': {
        std::stringstream str;
        while ((c = m_input.get()) != '"') {
          str.put(c);
        }
        m_token = std::make_shared<Token::String>(str.str());
        break;
      }

      case '\'': {
        int number = m_input.get();
        assert(m_input.get() == '\'');
        m_token = std::make_shared<Token::Number>(number);
        break;
      }

      default:
        if (isnumber(c)) {
          int number = 0;
          do {
            number *= 10;
            number += c - '0';
          } while (isnumber(c = m_input.get()));

          m_input.unget();

          m_token = std::make_shared<Token::Number>(number);
        } else if (isalpha(c) || c == '_') {
          std::stringstream id;
          do {
            id << c;
          } while (isalpha(c = m_input.get()) || isnumber(c) || c == '_' || c == '-');

          m_input.unget();

          m_token = std::make_shared<Token::ID>(id.str());
        } else {
          // TODO: proper error here
          std::cerr << "Invalid token `" << c << "`\n";
          throw;
        }
    }

    m_token->loc.start = start;
    m_token->loc.end = m_input.tellg();
    return m_token;
  }

  std::shared_ptr<Token> Lexer::token(Token::Type type) {
    auto token = m_token;
    ensure(type);
    return token;
  }

  std::shared_ptr<Token> Lexer::token(void) {
    return m_token;
  }

  void Lexer::ensure(Token::Type type) {
    if (m_token->type != type) {
      std::cerr << "Invalid token found: expected `" << Token::typeName(m_token->type) << "` to be `" << Token::typeName(type) << "`" << "\n";
      printSource();
      throw "Parser error";
    }
    nextToken();
  }

   void Lexer::invalidType() {
    if (m_token->type == Token::Type::END) {
      std::cerr << "Unexpected end of input\n";
    } else {
      int line = 1;
      int column = 0;
      m_input.seekg(0);
      std::stringstream _source;
      _source << m_input.rdbuf();
      std::string source = _source.str();
      for (int i = 0; i < m_token->loc.start; i++) {
        if (source[i] == '\n') {
          line++;
          column = 0;
        } else {
          column++;
        }
      }

      std::cerr << "Unexpected token `" << Token::typeName(m_token->type) << "` at " << line << ":" << column << std::endl;
    }
    printSource();
    throw "Type error";
  }

  void Lexer::printSource() {
    int start = m_token->loc.start;
    int actualStart = start;
    do {
      m_input.clear();
      m_input.seekg(start);
    } while (start > 0 && m_input.get() != '\n' && start--);

    char line[256];
    m_input.getline(line, 256);
    line[m_input.gcount()] = 0;
    std::cerr << line << "\n";
    std::cerr << std::setw(actualStart - start + 2) << "^\n";
  }
}
