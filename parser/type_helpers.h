#include "type.h"
#include "ast.h"
#include "type_error.h"

#pragma once

namespace Verve {
std::string uniqueName(const std::string &name, EnvPtr env);
std::string generic(const std::string &name, EnvPtr env);
void loadGenerics(const std::vector<std::string> &generics, EnvPtr env);
Type *simplify(Type *type, EnvPtr env);
bool typeEq(Type *expected, Type *actual, EnvPtr env);
Type *enumRetType(const TypeFunction *fnType, EnvPtr env);
TypeFunction *typeCheckArguments(const std::vector<AST::NodePtr> &arguments, const TypeFunction *fnType, EnvPtr env, const Loc &loc);
bool usesInterface(Type *t, EnvPtr env);
}
