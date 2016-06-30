#include "scope.h"

verve::Scope **verve::Scope::s_scopePool = NULL;
unsigned verve::Scope::s_scopePoolIndex = 0;
unsigned verve::Scope::s_scopePoolSize = 0;
