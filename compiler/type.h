#include "macros.h"

#include <sstream>
#include <unordered_map>
#include <vector>

#ifndef CEOS_TYPE_H
#define CEOS_TYPE_H

// data Maybe a = Just a | Nothing
// type :: [(Maybe Char -> Maybe Int)] -> [Maybe Char] -> [[Maybe Int]]
// List(Maybe(Char) -> Maybe(Int)) -> List(Maybe(Char)) -> List(List(Maybe(Int)))

// Nothing :: Maybe *
// Just :: a -> Maybe a // Just(1) => Maybe Int // Just(1.0) => Maybe Float

namespace ceos {

  struct Type { 
    virtual std::string toString() = 0;
    virtual ~Type() {}
  };

  struct BasicType : Type {
    BasicType(std::string &&n) : typeName(std::move(n)) {}

    virtual std::string toString() override {
      return typeName;
    }

    std::string typeName;
  };

  struct GenericType : BasicType {
    GenericType(std::string n) : BasicType(std::move(n)) {}
  };

  struct DataType : BasicType {
    DataType(std::string &&n , unsigned l) :
      BasicType(std::move(n)),
      length(l) {}

    unsigned length;
  };

  struct DataTypeInstance : Type {
    DataTypeInstance(DataType *dt, Type **ts) :
      dataType(dt)
    {
      for (unsigned i = 0; i < dt->length; i++) {
        types.push_back(ts[i]);
      }
    }

    virtual std::string toString() override {
      std::stringstream str;
      str << dataType->toString();
      str << "(";
      bool first = true;
      for (auto type : types) {
        if (!first) {
          str << ", ";
        }
        first = false;
        str << type->toString();
      }
      str << ")";
      return str.str();
    }

    DataType *dataType;
    std::vector<Type *> types;
  };

  struct TypeChain : Type {
    TypeChain() {}

    virtual std::string toString() override {
      std::stringstream str;
      for (unsigned i = 0; i < types.size(); i++) {
        if (i > 0) {
          str << " -> ";
        }

        str << types[i]->toString();
      }
      return str.str();
    }

    Type *returnType() { return types.back(); }
    std::vector<Type *> types;
  };

  struct TypeImplementation;
  struct TypeInterface : Type {
    virtual std::string toString() override {
      std::stringstream str;
      str << "interface "
          << name
          << "<"
          << genericName
          << ">";

      str << "{\n";
      for (auto it : functions) {
        str << it.first << " :: " << it.second->toString() << "\n";
      }
      str << "}";

      return str.str();
    }

    std::string name;
    std::string genericName;
    std::unordered_map<Type *, TypeImplementation *> implementations;
    std::unordered_map<std::string, TypeChain *> functions;
  };

  struct TypeImplementation {
    TypeInterface *interface;
    Type *type;
  };
}

#endif
