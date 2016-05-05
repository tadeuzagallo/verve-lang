#include <cstdio>
#include <cstdlib>
#include <cstring>

#ifndef CEOS_STRING_H
#define CEOS_STRING_H

namespace ceos {

class String {
  public:
  inline String(const char *str) {
    if (str) {
      m_str = dedupe(str);
    } else {
      m_str = NULL;
    }
  }

  inline const char *str() const {
    return m_str;
  }
  
  inline operator const char *() {
    return m_str;
  }

  inline bool operator==(String &other) {
    return m_str == other.m_str;
  }

  private:
    static inline unsigned hash(const char *str) {
      unsigned long hash = 5381;
      int c;
      while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c;
      }
      return hash;
    }

  static inline const char *dedupe(const char *str) {
    if (!s_strings) {
      s_size = s_initialSize;
      s_strings = (Entry *)calloc(s_size, 8);
    }

    unsigned hash = String::hash(str);
    unsigned index = hash % s_size;
    unsigned begin = index;

    Entry *e;
    while ((e = &s_strings[index])->str != NULL) {
      if (e->hash == hash && strcmp(e->str, str) == 0) {
        return e->str;
      } 
      if ((index = (index + 1) % s_size) == begin) break;
    }

    if (e->str == NULL) {
      e->hash = hash;
      e->str = str;
    } else {
      fputs("No space for strings left :(", stderr);
      throw;
    }

    return str;
  }

  struct Entry {
    unsigned hash;
    const char *str;
  };
  static const unsigned s_initialSize = 64;
  static unsigned s_size;
  static Entry *s_strings;

  const char *m_str;
};

}
#endif
