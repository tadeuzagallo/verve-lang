#include <cstdarg>
#include <cstdio>

namespace Verve {

class TypeError : public std::exception {
public:
  TypeError(Loc loc, const char *format, ...) :
    m_loc(loc)
  {
    va_list args1;
    va_list args2;
    va_start(args1, format);
    va_start(args2, format);
    auto length = vsnprintf(NULL, 0, format, args1);
    m_msg = (char *)malloc(length + 1);
    vsnprintf(m_msg, length + 1, format, args2);
    va_end(args1);
    va_end(args2);
  }

  ~TypeError() {
    free(m_msg);
  }

  virtual const char *what() const noexcept {
    return m_msg;
  }

  Loc &loc() {
    return m_loc;
  }

private:
  Loc m_loc;
  char *m_msg;
};

}
