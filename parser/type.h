#include "utils/macros.h"

#include <sstream>
#include <unordered_map>
#include <vector>

#pragma once

namespace ceos {

  struct Type;

  typedef std::unordered_map<std::string, Type *> TypeMap;

  struct Type {
    virtual bool equals(Type *) = 0;
    virtual std::string toString() = 0;
    virtual ~Type() {}
  };

  struct BasicType : Type {
    BasicType(std::string &&n) : typeName(std::move(n)) {}

    virtual bool equals(Type *other) override {
      BasicType *t;
      if (!(t = dynamic_cast<BasicType *>(other))) {
        return false;
      }
      return typeName == t->typeName;
    }

    virtual std::string toString() override {
      return typeName;
    }

    std::string typeName;
  };

  struct GenericType : BasicType {
    GenericType(std::string str) :
      BasicType(std::move(str)) {}
  };

  struct DataType : BasicType {
    using BasicType::BasicType;

    unsigned length;
  };

  struct DataTypeInstance : Type {
    virtual bool equals(Type *other) override {
      DataTypeInstance *t;
      if (!(t = dynamic_cast<DataTypeInstance *>(other))) {
        return false;
      }
      if (!dataType->equals(t->dataType)) {
        return false;
      }
      if (types.size() != t->types.size()) {
        return false;
      }
      for (unsigned i = 0; i < types.size(); i++) {
        if (!types[i]->equals(t->types[i])) {
          return false;
        }
      }
      return true;
    }

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

    DataType *dataType;
    std::vector<Type *> types;
  };

  struct TypeInterface;
  struct TypeFunction : Type {
    virtual bool equals(Type *other) override {
      TypeFunction *t;
      if (!(t = dynamic_cast<TypeFunction *>(other))) {
        return false;
      }
      for (unsigned i = 0; i < types.size(); i++) {
        if (!types[i]->equals(t->types[i])) {
          return false;
        }
      }
      return true;
    }

    virtual std::string toString() override {
      std::stringstream str;
      str << "(";
      for (unsigned i = 0; i < types.size(); i++) {
        if (i > 0) {
          str << " -> ";
        }

        str << types[i]->toString();
      }
      str << ")";
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

  struct TypeImplementation;
  struct TypeInterface : Type {
    virtual bool equals(Type *other) override {
      TypeInterface *t;
      if (!(t = dynamic_cast<TypeInterface *>(other))) {
        return false;
      }
      return name == t->name;
    }

    virtual std::string toString() override {
      std::stringstream str;
      str << "interface "
          << name
          << "<"
          << genericTypeName
          << "> {\n";

      for (auto it : functions) {
        str << it.first << " :: " << it.second->toString() << "\n";
      }
      str << "}";

      return str.str();
    }

    std::string name;
    std::string genericTypeName;
    std::vector<TypeImplementation *> implementations;
    std::unordered_map<std::string, TypeFunction *> functions;
  };

  struct TypeImplementation {
    TypeInterface *interface;
    Type *type;
  };
}
