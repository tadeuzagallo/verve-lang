#include "utils/macros.h"

#include "environment.h"
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
      BinaryOperation, \
      UnaryOperation, \
      List, \
      Match, \
      Case, \
      Pattern, \
      Let, \
      Constructor, \
      Assignment

namespace Verve {
  struct Generator;

namespace AST {
  ENUM_CLASS(Type, AST_TYPES)
  EVAL(MAP(DECLARE_TYPE, AST_TYPES))

  struct Node {
    Type type;
    Loc loc;

    Node(Loc l): loc(l) {  }

    virtual void generateBytecode(__unused Generator *gen) {
      throw std::runtime_error("Trying to generate bytecode for virtual node");
    }

    virtual ::Verve::Type *typeof(__unused EnvPtr env) {
      throw std::runtime_error("Trying to get type for virtual node");
    }
  };

  struct Program : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);

    BlockPtr body;
  };

  struct Block : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    std::vector<NodePtr> nodes;
    unsigned stackSlots = 0;
    EnvPtr env;
  };

  struct Number : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    int value;
  };

  struct Identifier : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    std::string name;
    std::string ns;
    bool isCaptured;
  };

  struct String : public Identifier {
    using Identifier::Identifier;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);
  };

  struct FunctionParameter : public Identifier {
    using Identifier::Identifier;

    virtual void generateBytecode(Generator *gen);

    unsigned index;
  };

  struct Call : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    NodePtr callee;
    std::vector<NodePtr> arguments;
  };

  struct Function : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    std::string name;
    std::string ns;
    std::vector<FunctionParameterPtr> parameters;
    BlockPtr body;
    bool needsScope;
    bool capturesScope;
  };

  struct If : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    NodePtr condition;
    BlockPtr ifBody;
    BlockPtr elseBody;
  };

  struct BinaryOperation : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    unsigned op;
    NodePtr lhs;
    NodePtr rhs;
  };

  struct UnaryOperation : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    unsigned op;
    NodePtr operand;
  };

  struct List : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    std::vector<NodePtr> items;
  };

  struct Pattern : public Node {
    using Node::Node;

    virtual void generateBytecode(__unused Generator *gen) {
      throw std::runtime_error("Implemented inline");
    }

    unsigned tag;
    std::string constructorName;
    std::vector<IdentifierPtr> values;
  };

  struct Case : public Node {
    using Node::Node;

    virtual void generateBytecode(__unused Generator *gen) {
      throw std::runtime_error("Implemented inline");
    }
    virtual ::Verve::Type *typeof(EnvPtr env);

    PatternPtr pattern;
    BlockPtr body;
  };

  struct Match : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    NodePtr value;
    std::vector<CasePtr> cases;
  };

  struct Assignment : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    NodePtr left;
    NodePtr value;
  };

  struct Let : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    std::vector<AssignmentPtr> assignments;
    BlockPtr block;
  };

  struct Constructor : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual ::Verve::Type *typeof(EnvPtr env);

    std::string name;
    std::vector<NodePtr> arguments;
    unsigned tag;
    unsigned size;
  };

  EVAL(MAP(DECLARE_CONVERTER, AST_TYPES))
  EVAL(MAP(DECLARE_CTOR, AST_TYPES))
}
}
