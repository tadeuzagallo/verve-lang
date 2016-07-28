#include "ast_printer.h"

#include <cassert>
#include <cstdarg>
#include <iomanip>
#include <iostream>

#define BEGIN_NODE(__name) \
void Printer::visit##__name(__name *node) { \
  depth++; \
  print(depth, #__name " {\n");

#define END_NODE() \
  print(depth, "}\n"); \
  depth--; \
}

#define PRINT_CUSTOM(__name, __value) \
  print(depth + 1, #__name ": %s\n", __value);

#define PRINT_CUSTOM_NODE(__label, __name) \
  print(depth + 1, #__label ": "); \
  inlineNext = true; \
  if (node->__name) { \
    node->__name->visit(this); \
  } else { \
    print(depth + 1, "null\n"); \
  }

#define PRINT_NODE(__name) \
  PRINT_CUSTOM_NODE(__name, __name)

#define PRINT_STRING(__name) \
  print(depth + 1, #__name ": \"%s\",\n", node->__name.c_str());

#define PRINT_ARRAY(__name) \
  print(depth + 1, #__name ": [\n"); \
  depth++; \
  for (const auto &__n : node->__name) { \
    __n->visit(this); \
  } \
  depth--; \
  print(depth + 1, "]\n");

#define PRINT_SUPERCLASS(__name) \
  visit##__name(node);

// TODO: This is just wrong. There shouldn't be any arrays of strings in the first place.
#define PRINT_STRING_ARRAY(__name) \
  print(depth + 1, #__name ": [\n"); \
  for (const auto &__n : node->__name) { \
    print(depth + 2, "\"%s\"\n", __n.c_str()); \
  } \
  print(depth + 1, "]\n");

static const char *stringify(bool t) {
  return t ? "true" : "false";
}

namespace Verve {
namespace AST {

void Printer::dump(AST::NodePtr ast) {
  Printer printer;
  ast->visit(&printer);
}

void Printer::print(unsigned depth, const char *format, ...) {
  auto marginSize = depth * indentation;
  char margin[marginSize + 1];
  if (inlineNext) {
    margin[0] = '\0';
    inlineNext = false;
  } else {
    for (auto i = 0u; i < marginSize; i++) {
      margin[i] = ' ';
    }
    margin[marginSize] = 0;
  }

  va_list args;
  va_start(args, format);
  printf("%s", margin);
  vprintf(format, args);
  va_end(args);
}

BEGIN_NODE(Program)
  PRINT_SUPERCLASS(Block)
END_NODE()

BEGIN_NODE(Block) 
  PRINT_ARRAY(nodes)
END_NODE()

BEGIN_NODE(Number)
  PRINT_CUSTOM(value, std::to_string(node->value).c_str())
  PRINT_CUSTOM(isFloat, stringify(node->isFloat))
END_NODE()

BEGIN_NODE(Identifier)
  PRINT_STRING(name)
  PRINT_STRING(ns)
END_NODE()

BEGIN_NODE(String)
  PRINT_STRING(value)
END_NODE()

BEGIN_NODE(FunctionParameter)
  PRINT_STRING(name)
  PRINT_CUSTOM(index, std::to_string(node->index).c_str())
END_NODE()

BEGIN_NODE(Call)
  PRINT_NODE(callee)
  PRINT_ARRAY(arguments)
END_NODE()

BEGIN_NODE(If)
  PRINT_NODE(condition)
  PRINT_NODE(ifBody)
  PRINT_NODE(elseBody)
END_NODE()

BEGIN_NODE(BinaryOperation)
  PRINT_CUSTOM(op, reinterpret_cast<char *>(&node->op))
  PRINT_NODE(lhs)
  PRINT_NODE(rhs)
END_NODE()

BEGIN_NODE(UnaryOperation)
  PRINT_CUSTOM(op, reinterpret_cast<char *>(&node->op))
  PRINT_NODE(operand)
END_NODE()

BEGIN_NODE(List)
  PRINT_ARRAY(items)
END_NODE()

BEGIN_NODE(Pattern)
  PRINT_STRING(constructorName)
  PRINT_ARRAY(values)
END_NODE()

BEGIN_NODE(Case)
  PRINT_NODE(pattern)
  PRINT_NODE(body)
END_NODE()

BEGIN_NODE(Match)
  PRINT_NODE(value)
  PRINT_ARRAY(cases)
END_NODE()

BEGIN_NODE(Assignment)
  switch (node->kind) {
    case Assignment::Pattern:
      PRINT_CUSTOM_NODE(left, left.pattern);
      break;
    case Assignment::Identifier:
      PRINT_CUSTOM_NODE(left, left.ident);
      break;
    default:
      assert(false);
  }
  PRINT_NODE(value)
END_NODE()

BEGIN_NODE(Let)
  PRINT_ARRAY(assignments)
  PRINT_NODE(block)
END_NODE()

BEGIN_NODE(Constructor)
  PRINT_STRING(name)
  PRINT_ARRAY(arguments)
END_NODE()

BEGIN_NODE(Interface)
  PRINT_STRING(name)
  PRINT_STRING(genericTypeName)
  PRINT_ARRAY(functions)
END_NODE()

BEGIN_NODE(Implementation)
  PRINT_STRING(interfaceName)
  PRINT_NODE(type)
  PRINT_ARRAY(functions)
END_NODE()

BEGIN_NODE(BasicType)
  PRINT_STRING(name)
END_NODE()

BEGIN_NODE(FunctionType)
  PRINT_STRING_ARRAY(generics)
  PRINT_ARRAY(params)
  PRINT_NODE(returnType)
END_NODE()

BEGIN_NODE(TypeConstructor)
  PRINT_STRING(name)
  PRINT_ARRAY(types)
END_NODE()

BEGIN_NODE(DataType)
  PRINT_STRING(name)
  PRINT_ARRAY(params)
END_NODE()

BEGIN_NODE(EnumType)
  PRINT_STRING(name)
  PRINT_STRING_ARRAY(generics)
  PRINT_ARRAY(constructors)
END_NODE()

BEGIN_NODE(Prototype)
  PRINT_STRING(name)
  PRINT_CUSTOM(isExternal, stringify(node->isExternal))
  PRINT_CUSTOM(isVirtual, stringify(node->isVirtual))
  PRINT_SUPERCLASS(FunctionType)
END_NODE()

BEGIN_NODE(Function)
  PRINT_STRING(ns)
  PRINT_STRING(name)
  PRINT_NODE(type)
  PRINT_ARRAY(parameters)
  PRINT_NODE(body)
  PRINT_CUSTOM(needsScope, stringify(node->needsScope))
  PRINT_CUSTOM(capturesScope, stringify(node->capturesScope))
END_NODE()

}
}
