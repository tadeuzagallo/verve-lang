#include "lexer.h"

#include "token.h"

#include <cassert>
#include <iomanip>
#include <iostream>
#include <sstream>

#define BASIC_TOKEN(CHAR, TYPE) \
  case CHAR: \
    m_token = new Token(Token::Type::TYPE); \
    break;

namespace ceos {

  char Lexer::nextChar() {
    char c = m_input[m_pos];
    if (c != '\0') {
      m_pos++;
    }
    return c;
  }

  void Lexer::nextToken() {
    char c;

    do {
      c = nextChar();
    } while(isspace(c));

    if (m_prevToken && m_prevToken != m_token) {
      delete m_prevToken;
    }

    m_prevToken = m_token;
    int start = m_pos - 1;
    switch (c) {
      BASIC_TOKEN('(', L_PAREN)
      BASIC_TOKEN(')', R_PAREN)
      BASIC_TOKEN('{', L_BRACE)
      BASIC_TOKEN('}', R_BRACE)
      BASIC_TOKEN('<', L_ANGLE)
      BASIC_TOKEN('>', R_ANGLE)
      BASIC_TOKEN(',', COMMA)

      case ':':
        if (nextChar() == ':') {
          m_token = new Token(Token::Type::TYPE);
        } else {
          m_pos--;
          m_token = new Token(Token::Type::COLON);
        }
        break;

      case '-':
        assert(nextChar() == '>');
        m_token = new Token(Token::Type::ARROW);
        break;

      case '\0':
        start = m_token->loc.end > 0 ? m_token->loc.end - 1 : 0;
        m_token = new Token(Token::Type::END);
        break;

      case '#':
        do {
          c = nextChar();
        } while (c != '\n');
        return nextToken();

      case '"': {
        std::stringstream str;
        while ((c = nextChar()) != '"') {
          str.put(c);
        }
        m_token = new Token::String(str.str());
        break;
      }

      case '\'': {
        int number = nextChar();
        assert(nextChar() == '\'');
        m_token = new Token::Number(number);
        break;
      }

      default:
        if (isnumber(c)) {
          int number = 0;
          do {
            number *= 10;
            number += c - '0';
          } while (isnumber(c = nextChar()));

          m_pos--;

          m_token = new Token::Number(number);
        } else if (isalpha(c) || c == '_') {
          std::stringstream id;
          do {
            id << c;
          } while (isalpha(c = nextChar()) || isnumber(c) || c == '_' || c == '-');

          m_pos--;

          m_token = new Token::ID(id.str());
        } else {
          // TODO: proper error here
          std::cerr << "Invalid token `" << c << "`\n";
          throw;
        }
    }

    m_token->loc.start = start;
    m_token->loc.end = m_pos;
  }

  Token *Lexer::token(Token::Type type) {
    ensure(type);
    return m_prevToken;
  }

  void Lexer::rewind() {
    m_token = m_prevToken;
    m_prevToken = nullptr;
    m_pos = m_token->loc.end;
  }

  bool Lexer::skip(Token::Type type) {
    if (m_token->type == type) {
      nextToken();
      return true;
    }
    return false;
  }

  Token *Lexer::token(void) {
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
      m_pos = 0;
      size_t pos = m_token->loc.start > m_offset ? m_token->loc.start - m_offset : m_token->loc.start;
      for (size_t i = 0; i < pos; i++) {
        if (m_input[i] == '\n') {
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
      m_pos = start;
    } while (start > 0 && nextChar() != '\n' && start--);

    char line[256];
    unsigned i = 0;
    while ((line[i++] = nextChar()) != '\n');
    line[i] = 0;
    std::cerr << line;
    std::cerr << std::setw(actualStart - start + 1) << "^\n";
  }
}
