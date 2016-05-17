#include "utils/macros.h"
#include "token.h"
#include "type.h"

#include <cassert>
#include <functional>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#ifndef CEOS_AST_H
#define CEOS_AST_H

static unsigned str_uid = 0;

#define AST_TYPES(__name, __options...) \
  ENUM_CLASS(__name, __options) \
  EVAL(MAP(DECLARE_CLASS, __options)) \
  EVAL(MAP(DECLARE_CONVERTER, __options))

#define DECLARE_CLASS(__class) class __class;

#define DECLARE_CONVERTER(__class) \
  static std::shared_ptr<AST::__class> as##__class(std::shared_ptr<AST> __n) { \
    assert(__n->type == Type::__class); \
    return std::static_pointer_cast<AST::__class>(__n); \
  }

namespace ceos {

  class AST {
    public:
      AST_TYPES(Type,
        Program,
        Block,
        Call,
        Number,
        ID,
        String,
        Function,
        FunctionArgument,
        If
      );

      AST(Type t) : type(t) {}

      ~AST() {
        if (typeInfo) {
          //delete typeInfo;
        }
      }

      Type type;
      Loc loc;
      ::ceos::Type *typeInfo;
  };

  class AST::Block : public AST {
    public:
    Block() : AST(Type::Block) {}

    std::vector<std::shared_ptr<AST>> nodes;
    std::function<void()> prologue;
    bool needsScope;
    bool capturesScope;
  };

  class AST::Program : public AST {
    public:
      Program() : AST(Type::Program) {}

      std::vector<std::shared_ptr<AST::Function>> functions;
      std::vector<std::string> strings;
      std::shared_ptr<AST::Block> body;
  };

  class AST::Number : public AST {
    public:
      Number(int v) : AST(Type::Number), value(v) {}

      int value;
  };

  class AST::ID : public AST {
    public:
      ID(std::string n, unsigned id) : AST(Type::ID), name(n), uid(id) { }

      std::string name;
      unsigned uid;
  };

  class AST::String : public AST {
    public:
      String(std::string n, unsigned id) : AST(Type::String), name(n), uid(id) { }

      std::string name;
      unsigned uid;
  };

  class AST::Call : public AST {
    public:
      Call() : AST(Type::Call) {}

      std::shared_ptr<AST> callee;
      std::vector<std::shared_ptr<AST>> arguments;
  };

  class AST::Function : public AST {
    public:
      Function() : AST(Type::Function) {}

      TypeChain *getTypeInfo() {
        return (TypeChain *)typeInfo;
      }

      std::shared_ptr<AST::ID> name;
      std::vector<std::shared_ptr<AST::FunctionArgument>> arguments;
      std::shared_ptr<AST::Block> body;
  };

  class AST::FunctionArgument : public AST {
    public:
      FunctionArgument(std::string name, unsigned i) : AST(Type::FunctionArgument), name(name), index(i) {}

      std::string name;
      unsigned index;
      bool isCaptured;
  };

  class AST::If : public AST {
    public:
      If() : AST(Type::If) {}

      std::shared_ptr<AST> condition;
      std::shared_ptr<AST::Block> ifBody;
      std::shared_ptr<AST::Block> elseBody;
  };

}

#endif
