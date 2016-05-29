#include "type.h"

namespace ceos {

  bool TypeConstructor::accepts(Type *other) {
    if (owner) {
      return owner->accepts(other);
    }
    return false;
  }

}
