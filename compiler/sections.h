#include "macros.h"

#ifndef CEOS_SECTIONS_H
#define CEOS_SECTIONS_H

struct Section {
  static int const Header = 0xCE05;

  ENUM(Type,
    Strings,
    Functions,
    Code
  );
};

#endif
