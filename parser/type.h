#include "utils/macros.h"

#include "environment.h"

#include <sstream>
#include <unordered_map>
#include <vector>

#pragma once

namespace Verve {

  struct Type;
  struct EnumType;
  struct TypeInterface;
  struct TypeImplementation;

  struct Type {
    virtual bool accepts(Type *, EnvPtr) = 0;
    virtual std::string toString() = 0;
    virtual ~Type() {}
  };

  static std::string typeToString(Type *t) {
    if (t) {
      return t->toString();
    } else {
      return "null";
    }
  }

  struct BasicType : Type {
    BasicType(std::string &&n) : typeName(std::move(n)) {}

    virtual bool accepts(Type *, EnvPtr) override;

    virtual std::string toString() override {
      return typeName;
    }

    std::string typeName;
  };

  struct GenericType : BasicType {
    GenericType(std::string str) :
      BasicType(std::move(str)) {}

    virtual bool accepts(Type *, EnvPtr) override;

    virtual std::string toString() override {
      return Environment::reverseGenericMapping[typeName];
    }
  };

  struct TypeFunction : Type {
    virtual bool accepts(Type *, EnvPtr) override;

    virtual std::string toString() override {
      std::stringstream str;
      str << "(";
      for (unsigned i = 0; i < types.size(); i++) {
        if (i > 0) {
          str << ", ";
        }

        str << typeToString(types[i]);
      }
      str << ") -> " << typeToString(returnType);
      return str.str();
    }

    std::string name;
    std::vector<Type *> types;
    std::vector<std::string> generics;
    Type *returnType;
    struct {
      bool isExternal: 1;
      bool isVirtual: 1;
      bool usesInterface: 1;
    };
    TypeInterface *interface = nullptr;
  };

  struct TypeInterface : Type {
    virtual bool accepts(Type *, EnvPtr) override;

    virtual std::string toString() override {
      std::stringstream str;
      str << "interface "
          << name
          << "<"
          << Environment::reverseGenericMapping[genericTypeName]
          << ">";

      return str.str();
    }

    std::string name;
    std::string genericTypeName;
    std::vector<TypeImplementation *> implementations;
    std::vector<std::string> virtualFunctions;
    std::vector<std::string> concreteFunctions;
  };

  struct TypeImplementation {
    std::string toString() {
      std::stringstream str;
      str << interface->name
          << "<"
          << typeToString(type)
          << ">";

      return str.str();
    }

    TypeInterface *interface = nullptr;
    Type *type;
  };

  struct TypeConstructor : TypeFunction {
    virtual bool accepts(Type *, EnvPtr) override;

    virtual std::string toString() override {
      std::stringstream str;
      str << name
          << TypeFunction::toString();
      return str.str();
    }

    unsigned tag;
  };

  struct EnumType : Type {
    virtual bool accepts(Type *, EnvPtr) override;

    virtual std::string toString() override {
      std::stringstream str;
      str << name;

      return str.str();
    }

    std::string name;
    std::vector<TypeConstructor *> constructors;
    std::vector<std::string> generics;
  };

  struct DataTypeInstance : Type {
    virtual bool accepts(Type *, EnvPtr) override;

    virtual std::string toString() override {
      std::stringstream str;
      str << typeToString(dataType);
      str << "<";
      bool first = true;
      for (auto type : types) {
        if (!first) {
          str << ", ";
        }
        first = false;
        str << typeToString(type);
      }
      str << ">";
      return str.str();
    }

    Type *dataType;
    std::vector<Type *> types;
  };

}
