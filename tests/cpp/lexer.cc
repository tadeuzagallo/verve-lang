#include "parser/lexer.h"
#include "parser/token.h"

#include <stdio.h>
#include <stdlib.h>

namespace ceos {

class LexerTest {
  public:

  static void testRewind() {
    Lexer l("1 (", 4);
    l.token(Token::Type::NUMBER);
    l.ensure(Token::Type::L_PAREN);
    l.rewind();
    l.token(Token::Type::L_PAREN);
    l.ensure(Token::Type::END);
  }

  static void test() {
    testRewind();
  }

};

}

int main() {
  ceos::LexerTest::test();
  return 0;
}
