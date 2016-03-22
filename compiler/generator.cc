#include "generator.h"
#include "opcodes.h"
#include "sections.h"

#include <iostream>
#include <iomanip>

namespace ceos {
  std::stringstream &Generator::generate() {
    generateProgram(m_ast);
    m_output.seekg(0);
    return m_output;
  }

  void Generator::generateNode(std::shared_ptr<AST> node) {
    switch (node->type) {
      case AST::Type::Call:
        generateCall(std::static_pointer_cast<AST::Call>(node));
        break;
      case AST::Type::Number:
        generateNumber(std::static_pointer_cast<AST::Number>(node));
        break;
      case AST::Type::ID:
        generateID(std::static_pointer_cast<AST::ID>(node));
        break;
      case AST::Type::String:
        generateString(std::static_pointer_cast<AST::String>(node));
        break;
      case AST::Type::FunctionArgument:
        generateFunctionArgument(std::static_pointer_cast<AST::FunctionArgument>(node));
        break;
      default:
        throw "Unhandled Type";
    }
  }

  bool Generator::handleSpecialCall(std::shared_ptr<AST::Call> call) {
    if (call->arguments[0]->type == AST::Type::ID) {
      std::string callee = AST::asID(call->arguments[0])->name;

      if (callee == "defn" || callee == "lambda") {
        if (callee == "lambda") {
          static unsigned lambdaID = 0;
          std::stringstream lambdaName;
          lambdaName << "lambda$" << lambdaID++;
          m_ast->strings.push_back(lambdaName.str());
        } else {
          auto name = AST::asID(call->arguments[1])->name;
        }

        emitOpcode(Opcode::create_closure);
        write(m_ast->functions.size());

        if (callee == "defn") {
          emitOpcode(Opcode::bind);
        }

        m_ast->functions.push_back(call);
        return true;
      } else if (callee == "if") {
        generateIf(call);
        return true;
      }
    }

    return false;
  }

  void Generator::write(int data) {
    m_output.write(reinterpret_cast<char *>(&data), sizeof(data));
  }

  void Generator::write(const std::string &data) {
    m_output << data;
    m_output.put('\0');
  }

  void Generator::emitOpcode(Opcode::Type opcode) {
    write(opcode);
  }

  void Generator::emitJmp(Opcode::Type jmpType, std::shared_ptr<AST> &node)  {
    emitJmp(jmpType, node, false);
  }

  void Generator::emitJmp(Opcode::Type jmpType, std::shared_ptr<AST> &node, bool skipNextJump)  {
    emitOpcode(jmpType);
    unsigned beforePos = m_output.tellp();
    write(0); // placeholder

    generateNode(node);
    unsigned afterPos = m_output.tellp();
    m_output.seekp(beforePos);
    write((afterPos - beforePos)
        - 4 // sizeof the jump offset
        + (skipNextJump ? 8 : 0) // special case for if with else
    );
    m_output.seekp(afterPos);
  }

  void Generator::generateCall(std::shared_ptr<AST::Call> call) {
    if (handleSpecialCall(call)) {
      return;
    }

    for (unsigned i = call->arguments.size(); i > 0;) {
      generateNode(call->arguments[--i]);
    }

    emitOpcode(Opcode::call);
    write(call->arguments.size() - 1); // don't include the callee
  }

  void Generator::generateNumber(std::shared_ptr<AST::Number> number) {
    emitOpcode(Opcode::push);
    write(number->value);
  }

  void Generator::generateID(std::shared_ptr<AST::ID> id) {
    auto v = m_scope->get(id->name);
    if (v.get()) {
      generateNode(v);
    } else {
      emitOpcode(Opcode::lookup);
      write(id->uid);
    }
  }

  void Generator::generateString(std::shared_ptr<AST::String> str) {
    emitOpcode(Opcode::load_string);
    write(str->uid);
  }

  void Generator::generateFunctionArgument(std::shared_ptr<AST::FunctionArgument> arg) {
    emitOpcode(Opcode::push_arg);
    write(arg->index);
  }

  void Generator::generateFunction(std::shared_ptr<AST::Call> fn) {
    auto callee = AST::asID(fn->arguments[0])->name;
    int index;
    std::string name;
    if (callee == "defn") {
      index = 1;
      name = AST::asID(fn->arguments[1])->name;
    } else if (callee == "lambda") {
      index = 0;
      static unsigned lambdaID = 0;
      std::stringstream lambdaName;
      lambdaName << "lambda$" << lambdaID++;
      name = lambdaName.str();
    } else {
      throw;
    }

    write(INDEX_OF(m_ast->strings, name));
    write(AST::asCall(fn->arguments[index + 1])->arguments.size());

    m_scope = m_scope->create();

    int i = 0;
    for (auto arg : AST::asCall(fn->arguments[index + 1])->arguments) {
      std::string &name = AST::asID(arg)->name;
      m_scope->set(name, std::make_shared<AST::FunctionArgument>(i++));
      write(INDEX_OF(m_ast->strings, name));
    }

    for (unsigned ii = index + 2; ii < fn->arguments.size(); ii++) {
      generateNode(fn->arguments[ii]);
    }

    m_scope = m_scope->restore();

    write(Opcode::ret);
  }

  void Generator::generateIf(std::shared_ptr<AST::Call> iff) {
    unsigned size = iff->arguments.size();
    assert(size == 3 || size == 4);

    generateNode(iff->arguments[1]);

    emitJmp(Opcode::jz, iff->arguments[2], size == 4 ? 2 : 0);

    if (size == 4) {
      emitJmp(Opcode::jmp, iff->arguments[3]);
    }
  }

  void Generator::generateProgram(std::shared_ptr<AST::Program> program) {
    for (auto node : program->nodes()) {
      generateNode(node);
    }

    auto text = m_output.str();
    m_output = std::stringstream();

    if (program->functions.size()) {
      unsigned i = 0;
      while (i < program->functions.size()) {
        write(Section::FunctionHeader);
        generateFunction(program->functions[i++]);
      }
    }

    auto functions = m_output.str();
    m_output = std::stringstream();

    if (program->strings.size()) {
      write(Section::Header);
      write(Section::Strings);

      for (auto string : program->strings) {
        write(string);
      }
    }


    if (functions.length()) {
      write(Section::Header);
      write(Section::Functions);
      m_output << functions;
    }

    write(Section::Header);
    write(Section::Text);
    m_output << text;
  }

  static std::vector<std::string> strings;
  static size_t width;
  static std::string padding("  ");
  void Generator::disassemble(std::stringstream &bytecode) {
    size_t size = bytecode.str().length();
    width = std::ceil(std::log10(size + 1)) + 1;

#define WRITE(OFFSET, ...) \
    std::cout << "[" \
    <<  std::setfill(' ') << std::setw(width) << ((int)bytecode.tellg() - (OFFSET)) \
    << "] " << padding << __VA_ARGS__ << "\n"

read_section:
    READ_INT(bytecode, ceos);
    assert(ceos == 0xCE05);

    READ_INT(bytecode, section);

    switch (section) {
      case Section::Strings:
        padding = "";
        WRITE(8, "STRINGS:");
        padding = "  ";
        goto section_strings;
        break;
      case Section::Functions:
        padding = "";
        WRITE(8, "FUNCTIONS:");
        padding = "  ";
        goto section_functions;
        break;
      case Section::Text:
        padding = "";
        WRITE(8, "TEXT:");
        padding = "  ";
        goto section_code;
        break;
    }

section_strings:
    while (true) {
      READ_INT(bytecode, ceos);
      bytecode.seekg(-4, bytecode.cur);
      if (ceos == 0xCE05)  {
        goto read_section;
      }
      static unsigned str_index = 0;
      READ_STR(bytecode, str);
      strings.push_back(str);
      WRITE(str.length() + 1, "$" << str_index++ << ": " << str);
    }

section_functions: {
      READ_INT(bytecode, fn_header);
      assert(fn_header == Section::FunctionHeader);

      READ_INT(bytecode, fn_id);
      READ_INT(bytecode, arg_count);

      std::stringstream args;
      for (int i = 0; i < arg_count; i++) {
        READ_INT(bytecode, argID);
        args << "$" << i << ": " << strings[argID];
        if (i < arg_count - 1) args << ", ";
      }

      padding = "";
      WRITE(8, strings[fn_id] << "(" << args.str() << "):");
      padding = "  ";

      while (true) {
        READ_INT(bytecode, header);
        bytecode.seekg(-4, bytecode.cur);
        if (header == Section::FunctionHeader) {
          goto section_functions;
        } else if (header == Section::Header) {
          goto read_section;
        }

        READ_INT(bytecode, opcode);
        printOpcode(bytecode, static_cast<Opcode::Type>(opcode));
      }
    }

section_code:
    while (true) {
      READ_INT(bytecode, opcode);
      printOpcode(bytecode, static_cast<Opcode::Type>(opcode));
    }
    goto read_section;
  }

  void Generator::printOpcode(std::stringstream &bytecode, Opcode::Type opcode) {
    size_t size = bytecode.str().length();
    size_t width = std::ceil(std::log10(size + 1)) + 1;

    switch (opcode) {
      case Opcode::push: {
        READ_INT(bytecode, value);
        WRITE(8, "push 0x" << std::setbase(16) << value << std::setbase(10));
        break;
      }
      case Opcode::call: {
        READ_INT(bytecode, nargs);
        WRITE(8, "call (" << nargs << ")");
        break;
      }
      case Opcode::load_string: {
        READ_INT(bytecode, stringID);
        WRITE(8, "load_string $" << stringID);
        break;
      }
      case Opcode::lookup: {
        READ_INT(bytecode, id);
        WRITE(8, "lookup $" << id << "(" << strings[id] << ")");
        break;
      }
      case Opcode::create_closure: {
        READ_INT(bytecode, id);
        WRITE(8, "create_closure " << strings[id]);
        break;
      }
      case Opcode::jmp:  {
        READ_INT(bytecode, target);
        WRITE(8, "jmp [" << ((int)bytecode.tellg() + target) << "]");
        break;
      }
      case Opcode::jz: {
        READ_INT(bytecode, target);
        WRITE(8, "jz [" << ((int)bytecode.tellg() + target) << "]");
        break;
      }
      case Opcode::push_arg: {
        READ_INT(bytecode, arg);
        WRITE(8, "push_arg $" << arg);
        break;
      }
      case Opcode::ret: {
        WRITE(4, "ret");
        break;
      }
      case Opcode::bind:
        WRITE(4, "bind");
        break;
      default:
        std::cerr << "Unhandled opcode: " << Opcode::typeName(static_cast<Opcode::Type>(opcode)) << "\n";
        throw;
    }
  }
#undef WRITE
}
