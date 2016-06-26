#include <cassert>
#include <cmath>
#include <iostream>
#include <iomanip>

#include "disassembler.h"

namespace ceos {
  Disassembler::Disassembler(std::stringstream &&bytecode):
    m_bytecode(std::move(bytecode))
  {
    auto size = m_bytecode.str().length();
    m_width = std::ceil(std::log10(size + 1)) + 1;
  }

  Disassembler::HelperStream Disassembler::write(int offset) {
    std::cout
      << "["
      << std::setfill(' ')
      << std::setw(m_width)
      << ((int)m_bytecode.tellg() - (offset * WORD_SIZE))
      << "] "
      << m_padding;

    return HelperStream();
  }

  int64_t Disassembler::read() {
    int64_t value;
    m_bytecode.read(reinterpret_cast<char *>(&value), sizeof(value));
    return value;
  }

  std::string Disassembler::readStr() {
    std::stringstream dest;
    m_bytecode.get(*dest.rdbuf(), '\0');
    m_bytecode.clear();
    m_bytecode.ignore(1);
    return dest.str();
  }

  int Disassembler::calculateJmpTarget(int target) {
    return (int)m_bytecode.tellg() - (2 * WORD_SIZE) + target;
  }

  void Disassembler::dump() {
    auto ceos = read();
    assert(ceos == Section::Header);

    dumpStrings();
    dumpFunctions();
    dumpText();

  }

  void Disassembler::dumpStrings() {
    assert(read() == Section::Strings);

    m_padding = "";
    write(1) << "STRINGS:";
    m_padding = "  ";

    unsigned str_index = 0;
    while (true) {
      auto ceos = read();
      if (ceos == Section::Header)  {
        return;
      }
      m_bytecode.seekg(-sizeof(ceos), m_bytecode.cur);
      auto str = readStr();
      while (m_bytecode.peek() == '\1') {
        m_bytecode.get();
      }
      m_strings.push_back(str);
      write((float)(str.length() + 1)/WORD_SIZE) <<  "$" << str_index++ << ": " << str;
    }
  }

  void Disassembler::dumpFunctions() {
    assert(read() == Section::Functions);

    m_padding = "";
    write(1) << "FUNCTIONS:";
    m_padding = "  ";

    while (true) {
      auto header = read();
      if (header == Section::Header) {
        return;
      }
      assert(header == Section::FunctionHeader);

      auto fnID = read();
      auto argCount = read();

      m_functions.push_back(m_strings[fnID]);

      std::stringstream args;
      for (int i = 0; i < argCount; i++) {
        auto argID = read();
        if (i) args << ", ";
        args << "$" << i << ": " << m_strings[argID];
      }

      m_padding = "";
      write(2 + argCount) << m_strings[fnID] << "(" << args.str() << "):";
      m_padding = "  ";

      while (true) {
        auto header = read();
        m_bytecode.seekg(-sizeof(header), m_bytecode.cur);
        if (header == Section::FunctionHeader || header == Section::Header) {
          break;
        }

        auto opcode = read();
        printOpcode(static_cast<Opcode::Type>(opcode));
      }
    }
  }

  void Disassembler::dumpText() {
    assert(read() == Section::Text);

    m_padding = "";
    write(1) << "TEXT:";
    m_padding = "  ";

    // skip size of lookup table
    m_bytecode.seekg(WORD_SIZE, m_bytecode.cur);
    while (true) {
      auto opcode = read();
      if (m_bytecode.eof() || m_bytecode.fail()) {
        break;
      }
      printOpcode(static_cast<Opcode::Type>(opcode));
    }
  }

  void Disassembler::printOpcode(Opcode::Type opcode) {
    switch (opcode) {
      case Opcode::push: {
        auto value = read();
        write(1)
          << "push 0x"
          << std::setbase(16)
          << value
          << std::setbase(10);
        break;
      }
      case Opcode::call: {
        auto argc = read();
        write(1) << "call (" << argc << ")";
        break;
      }
      case Opcode::load_string: {
        auto stringID = read();
        write(1) << "load_string $" << m_strings[stringID];
        break;
      }
      case Opcode::lookup: {
        auto symbol = read();
        auto cacheSlot = read();
        write(2) << "lookup $" << symbol << "(" << m_strings[symbol] << ") [cacheSlot=" << cacheSlot << "]";
        break;
      }
      case Opcode::create_closure: {
        auto fnID = read();
        auto capturesScope = read() ? "true" : "false";
        write(2) << "create_closure " << m_functions[fnID] << " [capturesScope=" << capturesScope << "]";
        break;
      }
      case Opcode::jmp: {
        auto target = read();
        write(1) << "jmp [" << calculateJmpTarget(target) << "]";
        break;
      }
      case Opcode::jz: {
        auto target = read();
        write(1) << "jz [" << calculateJmpTarget(target) << "]";
        break;
      }
      case Opcode::push_arg: {
        auto argID = read();
        write(1) << "push_arg $" << argID;
        break;
      }
      case Opcode::put_to_scope: {
        auto argID = read();
        write(1) << "put_to_scope $" << m_strings[argID];
        break;
      }
      case Opcode::bind: {
        auto stringID = read();
        write(1) << "bind $" << m_strings[stringID];
        break;
      }
      case Opcode::alloc_obj: {
        auto size = read();
        auto tag = read();
        write(2) << "alloc_obj (size=" << size << ", tag=" << tag << ")";
        break;
      }
      case Opcode::alloc_list: {
        auto size = read();
        write(1) << "alloc_list (size=" << size << ")";
        break;
      }
      case Opcode::obj_store_at: {
        auto index = read();
        write(1) << "obj_store_at #" << index;
        break;
      }
      case Opcode::obj_tag_test: {
        auto tag = read();
        write(1) << "obj_tag_test #" << tag;
        break;
      }
      case Opcode::obj_load: {
        auto offset = read();
        write(1) << "obj_load #" << offset;
        break;
      }
      case Opcode::stack_alloc: {
        auto size = read();
        write(1) << "stack_alloc #" << size;
        break;
      }
      case Opcode::stack_store: {
        auto slot = read();
        write(1) << "stack_store #" << slot;
        break;
      }
      case Opcode::stack_load: {
        auto slot = read();
        write(1) << "stack_load #" << slot;
        break;
      }
      case Opcode::stack_free: {
        auto size = read();
        write(1) << "stack_free #" << size;
        break;
      }
      default:
        write() << Opcode::typeName(static_cast<Opcode::Type>(opcode));
    }
  }
}
