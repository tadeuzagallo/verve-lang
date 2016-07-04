#include "parser/lexer.h"
#include "parser/token.h"

#include <stdio.h>
#include <stdlib.h>

namespace Verve {

class LexerTest {
  public:

  static void testRewind() {
    Lexer l("", "1 (", 4);
    l.token(Token::Type::NUMBER);
    l.match('(');
    l.rewind();
    l.match('(');
    l.token(Token::Type::END);
  }

  static void testRewindToSpecificLoc() {
    Lexer l("", "1 (", 4);
    auto start = l.token().loc;
    l.token(Token::Type::NUMBER);
    l.match('(');
    l.token(Token::Type::END);
    l.rewind(start);
    l.token(Token::Type::NUMBER);
  }

  static void test() {
    testRewind();
    testRewindToSpecificLoc();
  }

};

}

int main() {
  Verve::LexerTest::test();
  return 0;
}
