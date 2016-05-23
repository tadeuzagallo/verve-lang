#include "utils/macros.h"
#include "token.h"

#include <cassert>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#pragma once

static unsigned str_uid = 0;

#define DECLARE_TYPE(__class) \
  struct __class; \
  typedef std::shared_ptr<__class> __class##Ptr;

#define DECLARE_CONVERTER(__class) \
  __unused static __class##Ptr as##__class(NodePtr __n) { \
    assert(__n->type == Type::__class); \
    return std::static_pointer_cast<__class>(__n); \
  }

#define DECLARE_CTOR(__class)  \
    static inline __class##Ptr create##__class(Loc loc = {0, 0}) { \
       auto node = std::make_shared<__class>(); \
      node->type = Type::__class; \
      node->loc = loc; \
      return node; \
    } \

#define AST_TYPES \
      Node, \
      Program, \
      Block, \
      Call, \
      Number, \
      Identifier, \
      String, \
      Function, \
      FunctionParameter, \
      If

namespace ceos {
namespace AST {

  ENUM_CLASS(Type, AST_TYPES)
  EVAL(MAP(DECLARE_TYPE, AST_TYPES))

  struct Node {
    Type type;
    Loc loc;
  };

  struct Program : public Node {
    BlockPtr body;
  };

  struct Block : public Node {
    std::vector<NodePtr> nodes;
    std::function<void()> prologue;
    bool needsScope;
    bool capturesScope;
  };

  struct Number : public Node {
    int value;
  };

  struct Identifier : public Node {
    std::string name;
    unsigned uid;
  };

  struct String : public Identifier {};

  struct FunctionParameter : public Identifier {
    bool isCaptured;
  };

  struct Call : public Node {
    NodePtr callee;
    std::vector<NodePtr> arguments;
  };

  struct Function : public Node {
    IdentifierPtr name;
    std::vector<FunctionParameterPtr> parameters;
    BlockPtr body;
  };

  struct If : public Node {
    NodePtr condition;
    BlockPtr ifBody;
    BlockPtr elseBody;
  };

  EVAL(MAP(DECLARE_CONVERTER, AST_TYPES))
  EVAL(MAP(DECLARE_CTOR, AST_TYPES))
}
}
