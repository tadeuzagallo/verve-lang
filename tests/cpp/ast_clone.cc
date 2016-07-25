#include "parser/ast.h"

#include <cassert>

namespace Verve {

class ASTCloneTest {
  public:

  static void testBasicClone() {
    auto number = AST::createNumber({0, 0});
    number->value = 1.5;
    number->isFloat = true;

    auto cp = asNumber(number->clone());
    cp->value = 1;
    cp->isFloat = false;

    assert(number->value == 1.5);
    assert(number->isFloat == true);
  }

  static void testDeepClone() {
    auto number = AST::createNumber({0, 0});
    number->value = 1.5;
    number->isFloat = true;

    auto block = AST::createBlock({0, 0});
    block->nodes.push_back(number);

    auto cp = asBlock(block->clone());

    number->value = 1;

    assert(asNumber(cp->nodes[0])->value == 1.5);
  }

  static void test() {
    testBasicClone();
    testDeepClone();
  }

};

}

int main() {
  Verve::ASTCloneTest::test();
  return 0;
}
