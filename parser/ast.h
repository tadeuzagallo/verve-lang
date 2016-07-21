#include "utils/macros.h"

#include "environment.h"
#include "token.h"
#include "type.h"

#include <cassert>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#pragma once

#define DECLARE_TYPE(__class) \
  struct __class; \
  typedef std::shared_ptr<__class> __class##Ptr;

#define DECLARE_CONVERTER(__class) \
  __unused static __class##Ptr as##__class(std::shared_ptr<NodeInterface> __n) { \
    return std::dynamic_pointer_cast<__class>(__n); \
  }

#define DECLARE_CTOR(__class)  \
    static inline __class##Ptr create##__class(Loc loc) { \
       auto node = std::make_shared<__class>(loc); \
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
      Assignment, \
      Interface, \
      Implementation, \
      AbstractType, \
      BasicType, \
      FunctionType, \
      DataType, \
      EnumType, \
      TypeConstructor, \
      Prototype

namespace Verve {
  struct Generator;
  struct ASTPrinter;

namespace AST {
  EVAL(MAP(DECLARE_TYPE, AST_TYPES))

  struct NodeInterface {
    virtual void generateBytecode(Generator *gen) = 0;
    virtual Type *typeof(EnvPtr env) = 0;
    virtual void naming(EnvPtr env) = 0;
    virtual void printAST(ASTPrinter &printer, unsigned depth) = 0;
  };

  struct FunctionInterface : virtual public NodeInterface {
    virtual std::string &getName() = 0;
  };


  struct Node : virtual public NodeInterface {
    Loc loc;

    Node(Loc l): loc(l) {  }

    virtual void generateBytecode(__unused Generator *gen) {
      throw std::runtime_error("Trying to generate bytecode for virtual node");
    }

    virtual Type *typeof(__unused EnvPtr env) {
      throw std::runtime_error("Trying to get type for virtual node");
    }

    virtual void naming(__unused EnvPtr env) {}

    virtual void printAST(__unused ASTPrinter &_, __unused unsigned depth) {
      throw std::runtime_error("Trying to print virtual node");
    }
  };

  struct Program : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    BlockPtr body;
    std::vector<ProgramPtr> imports;
  };

  struct Block : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::vector<NodePtr> nodes;
    unsigned stackSlots = 0;
    EnvPtr env;
  };

  struct Number : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    double value;
    bool isFloat = false;
  };

  struct Identifier : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::string name;
    std::string ns;
    bool isCaptured;
    bool isFunctionParameter = false;
    unsigned index;
  };

  struct String : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::string value;
  };

  struct FunctionParameter : public Identifier {
    using Identifier::Identifier;

    virtual void generateBytecode(Generator *gen);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);
  };

  struct Call : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    NodePtr callee;
    std::vector<NodePtr> arguments;
  };

  struct If : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    NodePtr condition;
    BlockPtr ifBody;
    BlockPtr elseBody = nullptr;
  };

  struct BinaryOperation : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    unsigned op;
    NodePtr lhs;
    NodePtr rhs;
  };

  struct UnaryOperation : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    unsigned op;
    NodePtr operand;
  };

  struct List : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::vector<NodePtr> items;
  };

  struct Pattern : public Node {
    using Node::Node;

    virtual void generateBytecode(__unused Generator *gen) {
      throw std::runtime_error("Implemented inline");
    }
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    unsigned tag;
    std::string constructorName;
    std::vector<IdentifierPtr> values;
    NodePtr value;
  };

  struct Case : public Node {
    using Node::Node;

    virtual void generateBytecode(__unused Generator *gen) {
      throw std::runtime_error("Implemented inline");
    }
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    PatternPtr pattern;
    BlockPtr body;
  };

  struct Match : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    NodePtr value;
    std::vector<CasePtr> cases;
  };

  struct Assignment : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    enum {
      Pattern,
      Identifier,
    } kind;

    union Left {
      Left() :
        pattern(nullptr) {}

      PatternPtr pattern;
      IdentifierPtr ident;

      ~Left() {
        pattern = nullptr;
        ident = nullptr;
      }
    } left;
    NodePtr value;
  };

  struct Let : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::vector<AssignmentPtr> assignments;
    BlockPtr block;
  };

  struct Constructor : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::string name;
    std::vector<NodePtr> arguments;
    unsigned tag;
    unsigned size;
  };

  struct Interface : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::string name;
    std::string genericTypeName;
    std::vector<std::string> virtualFunctions;
    std::vector<std::string> concreteFunctions;
    std::vector<std::shared_ptr<FunctionInterface>> functions;
    EnvPtr env;
  };

  struct Implementation : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::string interfaceName;
    AbstractTypePtr type;
    std::vector<std::shared_ptr<FunctionInterface>> functions;
    EnvPtr env;
  };

  struct AbstractType : public Node {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
  };

  struct BasicType : public AbstractType {
    using AbstractType::AbstractType;

    virtual Type *typeof(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::string name;
  };

  struct FunctionType : public AbstractType {
    using AbstractType::AbstractType;

    virtual Type *typeof(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::vector<std::string> generics;
    std::vector<AbstractTypePtr> params;
    AbstractTypePtr returnType;
  };

  struct DataType : public AbstractType {
    using AbstractType::AbstractType;

    virtual Type *typeof(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::string name;
    std::vector<AbstractTypePtr> params;
  };

  struct EnumType : public AbstractType {
    using AbstractType::AbstractType;

    virtual Type *typeof(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::string name;
    std::vector<std::string> generics;
    std::vector<TypeConstructorPtr> constructors;
  };

  struct TypeConstructor : public AbstractType {
    using AbstractType::AbstractType;

    virtual void printAST(ASTPrinter &printer, unsigned depth);

    std::string name;
    std::vector<AbstractTypePtr> types;
  };

  struct Prototype : public FunctionType, public FunctionInterface {
    using FunctionType::FunctionType;

    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void generateBytecode(__unused Generator *gen) {}
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    virtual std::string &getName() {
      return name;
    }

    std::string name;
    struct {
      bool isExternal: 1;
      bool isVirtual: 1;
    };
  };

  struct Function : public Node, public FunctionInterface {
    using Node::Node;

    virtual void generateBytecode(Generator *gen);
    virtual Type *typeof(EnvPtr env);
    virtual void naming(EnvPtr env);
    virtual void printAST(ASTPrinter &printer, unsigned depth);

    virtual std::string &getName() {
      return name;
    }

    PrototypePtr type = nullptr;
    std::string ns;
    std::string name;
    std::vector<FunctionParameterPtr> parameters;
    BlockPtr body;
    bool needsScope;
    bool capturesScope;
  };


  EVAL(MAP(DECLARE_CONVERTER, AST_TYPES))
  EVAL(MAP(DECLARE_CTOR, AST_TYPES))
}
}
