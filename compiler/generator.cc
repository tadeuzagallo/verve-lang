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
      case AST::Type::Function:
        generateFunctionDefinition(AST::asFunction(node));
        break;
      case AST::Type::If:
        generateIf(AST::asIf(node));
        break;
      case AST::Type::TypeInfo:
        // TODO: use type info
        break;
      default:
        std::cerr <<  "Unhandled node: `" << AST::typeName(node->type) << "`\n";
        throw;
    }
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

  void Generator::emitJmp(Opcode::Type jmpType, std::vector<std::shared_ptr<AST>> &body)  {
    emitJmp(jmpType, body, false);
  }

  void Generator::emitJmp(Opcode::Type jmpType, std::vector<std::shared_ptr<AST>> &body, bool skipNextJump)  {
    emitOpcode(jmpType);
    unsigned beforePos = m_output.tellp();
    write(0); // placeholder

    for (auto node : body) {
      generateNode(node);
    }

    unsigned afterPos = m_output.tellp();
    m_output.seekp(beforePos);
    write((afterPos - beforePos)
        - 4 // sizeof the jump offset
        + (skipNextJump ? 8 : 0) // special case for if with else
    );
    m_output.seekp(afterPos);
  }

  void Generator::generateCall(std::shared_ptr<AST::Call> call) {
    for (unsigned i = call->arguments.size(); i > 0;) {
      generateNode(call->arguments[--i]);
    }

    generateNode(call->callee);

    emitOpcode(Opcode::call);
    write(call->arguments.size());
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

  void Generator::generateFunctionDefinition(std::shared_ptr<AST::Function> fn) {
    emitOpcode(Opcode::create_closure);
    write(m_ast->functions.size());
    if (fn->name->name != "_") {
      emitOpcode(Opcode::bind);
    }
    m_ast->functions.push_back(fn);
  }

  void Generator::generateFunctionSource(std::shared_ptr<AST::Function> fn) {
    std::string fnName = fn->name->name;
    if (fnName == "_") {
      static unsigned id = 0;
      fnName = "_" + std::to_string(id++);
      m_ast->strings.push_back(fnName);
    }
    write(INDEX_OF(m_ast->strings, fnName));
    write(fn->arguments.size());

    m_scope = m_scope->create();

    int i = 0;
    for (auto arg : fn->arguments) {
      std::string &name = AST::asID(arg)->name;
      m_scope->set(name, std::make_shared<AST::FunctionArgument>(i++));
      write(INDEX_OF(m_ast->strings, name));
    }

    for (auto node : fn->body) {
      generateNode(node);
    }

    m_scope = m_scope->restore();

    write(Opcode::ret);
  }

  void Generator::generateIf(std::shared_ptr<AST::If> iff) {
    generateNode(iff->condition);

    emitJmp(Opcode::jz, iff->ifBody, iff->elseBody.size() > 0);

    if (iff->elseBody.size()) {
      emitJmp(Opcode::jmp, iff->elseBody);
    }
  }

  void Generator::generateProgram(std::shared_ptr<AST::Program> program) {
    for (auto node : program->nodes()) {
      generateNode(node);
    }

    auto text = m_output.str();
    m_output = std::stringstream();

    if (program->functions.size()) {
      for (unsigned i = 0; i < program->functions.size(); i++) {
        write(Section::FunctionHeader);
        generateFunctionSource(program->functions[i]);
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
  static std::vector<std::string> functions;
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
      case Section::Functions: {
        padding = "";
        WRITE(8, "FUNCTIONS:");
        padding = "  ";
        auto offset = bytecode.tellg();
        while (true) {
          READ_INT(bytecode, header);
          if (header == Section::FunctionHeader) {
            READ_INT(bytecode, fnID);
            functions.push_back(strings[fnID]);
            continue;
          } else if (header == Section::Header) {
            break;
          }
        }
        bytecode.seekg(offset);
        goto section_functions;
        break;
      }
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
        READ_INT(bytecode, fnID);
        WRITE(8, "create_closure " << functions[fnID]);
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
