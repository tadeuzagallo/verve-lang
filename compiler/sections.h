#include "macros.h"

#ifndef CEOS_SECTIONS_H
#define CEOS_SECTIONS_H

struct Section {
  static int const Header = 0xCE05;
  static int const FunctionHeader = 0xCE0F;

  ENUM(Type,
    Strings,
    Functions,
    Text,
  );
};

#endif
