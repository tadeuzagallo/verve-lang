#include "utils/macros.h"

#pragma once

struct Section {
  static int const Header = 0xCE05;
  static int const FunctionHeader = 0xCE0F;

  ENUM(Type,
    Strings,
    Functions,
    Text,
  );
};
