#include "utils/macros.h"
#include "token.h"

#include <cassert>
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
    static inline __class##Ptr create##__class(Loc loc) { \
       auto node = std::make_shared<__class>(loc); \
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
      If, \
      ObjectTagTest, \
      ObjectLoad, \
      StackStore, \
      StackLoad

namespace ceos {
namespace AST {

  ENUM_CLASS(Type, AST_TYPES)
  EVAL(MAP(DECLARE_TYPE, AST_TYPES))

  struct Node {
    Type type;
    Loc loc;

    Node(Loc l): loc(l) {  }
  };

  struct Program : public Node {
    using Node::Node;

    BlockPtr body;
  };

  struct Block : public Node {
    using Node::Node;

    std::vector<NodePtr> nodes;
    unsigned stackSlots = 0;
  };

  struct Number : public Node {
    using Node::Node;

    int value;
  };

  struct Identifier : public Node {
    using Node::Node;

    std::string name;
  };

  struct String : public Identifier {
    using Identifier::Identifier;
  };

  struct FunctionParameter : public Identifier {
    using Identifier::Identifier;

    unsigned index;
    bool isCaptured;
  };

  struct Call : public Node {
    using Node::Node;

    NodePtr callee;
    std::vector<NodePtr> arguments;
    bool isConstructor = false;
    unsigned tag;
    unsigned size;
  };

  struct Function : public Node {
    using Node::Node;

    std::string name;
    std::vector<FunctionParameterPtr> parameters;
    BlockPtr body;
    bool needsScope;
    bool capturesScope;
  };

  struct If : public Node {
    using Node::Node;

    NodePtr condition;
    BlockPtr ifBody;
    BlockPtr elseBody;
  };

  struct ObjectTagTest : public Node {
    using Node::Node;

    NodePtr object;
    unsigned tag;
  };

  struct ObjectLoad : public Node {
    using Node::Node;

    NodePtr object;
    std::string constructorName;
    unsigned offset;
  };

  struct StackStore : public Node {
    using Node::Node;

    unsigned slot;
    NodePtr value;
  };

  struct StackLoad : public Node {
    using Node::Node;

    unsigned slot;
    NodePtr value;
  };

  EVAL(MAP(DECLARE_CONVERTER, AST_TYPES))
  EVAL(MAP(DECLARE_CTOR, AST_TYPES))
}
}
