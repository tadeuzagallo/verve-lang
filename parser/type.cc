#include "type.h"

namespace ceos {
  virtual bool BasicType::accepts(Type *other) override {
    BasicType *t;
    if (!(t = dynamic_cast<BasicType *>(other))) {
      return false;
    }
    return typeName == t->typeName;
  }

  virtual bool DataTypeInstance::accepts(Type *other) override {
    DataTypeInstance *t;
    if (!(t = dynamic_cast<DataTypeInstance *>(other))) {
      return false;
    }
    if (!dataType->accepts(t->dataType)) {
      return false;
    }
    if (types.size() != t->types.size()) {
      return false;
    }
    for (unsigned i = 0; i < types.size(); i++) {
      if (!types[i]->accepts(t->types[i])) {
        return false;
      }
    }
    return true;
  }

  virtual bool TypeFunction::accepts(Type *other) override {
    TypeFunction *t;
    if (!(t = dynamic_cast<TypeFunction *>(other))) {
      return false;
    }
    for (unsigned i = 0; i < types.size(); i++) {
      if (!types[i]->accepts(t->types[i])) {
        return false;
      }
    }
    return true;
  }

  virtual bool TypeInterface::accepts(Type *other) override {
    for (auto impl : implementations) {
      if (impl->type->accepts(other)) {
        return true;
      }
    }
    return false;
  }

  bool TypeConstructor::accepts(Type *other) {
    if (owner) {
      return owner->accepts(other);
    }
    return false;
  }

  virtual bool EnumType::accepts(Type *other) override {
    if (auto otherCtor = dynamic_cast<TypeConstructor *>(other)) {
      return otherCtor->type->returnType == this;
    }
    return other == this;
  }

}
