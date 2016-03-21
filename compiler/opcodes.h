#include "macros.h"

#ifndef CEOS_OPCODES_H
#define CEOS_OPCODES_H

class Opcode {
  public:
    ENUM(Type,
      ret,
      bind,
      push,
      call,
      jz,
      jmp,
      create_lambda,
      load_string,
      push_arg,
      lookup
    );
};

#define READ_INT(FROM, TO) \
    int TO; \
    FROM.read(reinterpret_cast<char *>(&TO), sizeof(TO)); \
    if (FROM.eof() || FROM.fail()) return

#define READ_STR(FROM, TO) \
    std::stringstream TO##_; \
    FROM.get(*TO##_.rdbuf(), '\0'); \
    std::string TO = TO##_.str(); \
    FROM.ignore(1);

#define READ_STR(FROM, TO) \
    std::stringstream TO##_; \
    FROM.get(*TO##_.rdbuf(), '\0'); \
    std::string TO = TO##_.str(); \
    FROM.ignore(1);

#endif
