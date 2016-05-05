#include <cstdio>
#include <cstdlib>
#include <cstring>

#ifndef CEOS_STRING_H
#define CEOS_STRING_H
namespace ceos {

class String {
  public:
  String(const char *str) {
    m_str = dedupe(str);
  }

  const char *operator(const char *)() {
    return m_str;
  }

  private:

  static const char *dedupe(const char *str) {
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
      fputs(stderr, "No space for strings left :(");
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
