#include <string>

namespace ceos {
  class Parser;

  Parser parseFile(std::string filename, std::string dirname, std::string ns);
}
