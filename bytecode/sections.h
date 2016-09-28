#include "utils/macros.h"

#pragma once

struct Section {
  static int const Header = 0xCE05;
  static int const FunctionHeader = 0xCE0F;
  static int const TypeMapHeader = 0xCE01;

  ENUM(Type,
    Strings,
    TypeMaps,
    Functions,
    Text,
  );
};
