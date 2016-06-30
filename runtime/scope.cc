#include "scope.h"

Verve::Scope **Verve::Scope::s_scopePool = NULL;
unsigned Verve::Scope::s_scopePoolIndex = 0;
unsigned Verve::Scope::s_scopePoolSize = 0;
