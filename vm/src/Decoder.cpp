#include "Decoder.h"

namespace Verve {

Decoder::Decoder(std::ifstream&& is)
  : m_is(std::move(is))
{}

Decoder::~Decoder()
{
  m_is.close();
}

Bytecode Decoder::decode()
{
  Bytecode bytecode;
  return bytecode;
}

}
