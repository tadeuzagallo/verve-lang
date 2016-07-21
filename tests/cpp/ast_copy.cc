#include "parser/ast.h"

#include <cassert>

namespace Verve {

class ASTCopyTest {
  public:

  static void testBasicCopy() {
    auto number = AST::createNumber({0, 0});
    number->value = 1.5;
    number->isFloat = true;

    auto cp = asNumber(number->copy());
    cp->value = 1;
    cp->isFloat = false;

    assert(number->value == 1.5);
    assert(number->isFloat == true);
  }

  static void testDeepCopy() {
    auto number = AST::createNumber({0, 0});
    number->value = 1.5;
    number->isFloat = true;

    auto block = AST::createBlock({0, 0});
    block->nodes.push_back(number);

    auto cp = asBlock(block->copy());

    number->value = 1;

    assert(asNumber(cp->nodes[0])->value == 1.5);
  }

  static void test() {
    testBasicCopy();
    testDeepCopy();
  }

};

}

int main() {
  Verve::ASTCopyTest::test();
  return 0;
}
