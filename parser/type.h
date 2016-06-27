#include "utils/macros.h"

#include "environment.h"

#include <sstream>
#include <unordered_map>
#include <vector>

#pragma once

namespace ceos {

  struct Environment;
  struct Type;
  struct EnumType;
  struct TypeInterface;

  typedef std::unordered_map<std::string, Type *> TypeMap;

  struct Type {
    virtual bool accepts(Type *, EnvPtr) = 0;
    virtual std::string toString() = 0;
    virtual ~Type() {}
  };

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

        str << types[i]->toString();
      }
      str << ") -> " << returnType->toString();
      return str.str();
    }

    std::string name;
    std::vector<std::string> generics;
    std::vector<Type *> types;
    Type *returnType;
    struct {
      bool isExternal: 1;
      bool isVirtual: 1;
    };
    TypeInterface *interface;
  };

  struct TypeImplementation {
    TypeInterface *interface;
    Type *type;
  };

  struct TypeInterface : Type {
    virtual bool accepts(Type *, EnvPtr) override;

    virtual std::string toString() override {
      std::stringstream str;
      str << "interface "
          << name
          << "<"
          << genericTypeName
          << ">";

      return str.str();
    }

    std::string name;
    std::string genericTypeName;
    std::vector<TypeImplementation *> implementations;
    std::unordered_map<std::string, TypeFunction *> functions;
  };

  struct TypeConstructor : Type {
    virtual bool accepts(Type *, EnvPtr) override;

    virtual std::string toString() override {
      std::stringstream str;
      str << type->name
          << type->toString();

      return str.str();
    }

    TypeFunction *type;
    EnumType *owner = nullptr;
    unsigned tag;
    unsigned size = 0;
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
      str << dataType->toString();
      str << "<";
      bool first = true;
      for (auto type : types) {
        if (!first) {
          str << ", ";
        }
        first = false;
        str << type->toString();
      }
      str << ">";
      return str.str();
    }

    Type *dataType;
    std::vector<Type *> types;
  };

}
