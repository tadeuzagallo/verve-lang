#include "scope.h"

ceos::Scope **ceos::Scope::s_scopePool = NULL;
unsigned ceos::Scope::s_scopePoolIndex = 0;
unsigned ceos::Scope::s_scopePoolSize = 0;
