#include "generator.h"
#include "opcodes.h"
#include "sections.h"

#include <iostream>
#include <iomanip>

#define WORD_SIZE 8

namespace ceos {
  static unsigned lookupID = 1;
  static bool capturesScope = true;

  std::stringstream &Generator::generate() {
    generateProgram(m_ast);
    emitOpcode(Opcode::exit);
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
      case AST::Type::Block:
        generateBlock(AST::asBlock(node));
        break;
      default:
        std::cerr <<  "Unhandled node: `" << AST::typeName(node->type) << "`\n";
        throw;
    }
  }

  void Generator::write(int64_t data) {
    m_output.write(reinterpret_cast<char *>(&data), sizeof(data));
  }

  void Generator::write(const std::string &data) {
    m_output << data;
    m_output.put(0);
  }

  void Generator::emitOpcode(Opcode::Type opcode) {
    write(Opcode::opcodeAddress(opcode));
  }

  void Generator::emitJmp(Opcode::Type jmpType, std::shared_ptr<AST::Block> &body)  {
    emitJmp(jmpType, body, false);
  }

  void Generator::emitJmp(Opcode::Type jmpType, std::shared_ptr<AST::Block> &body, bool skipNextJump)  {
    emitOpcode(jmpType);
    unsigned beforePos = m_output.tellp();
    write(0); // placeholder

    generateNode(body);

    unsigned afterPos = m_output.tellp();
    m_output.seekp(beforePos);
    write((afterPos - beforePos)
        + (skipNextJump ? (3 * WORD_SIZE) : WORD_SIZE) // special case for if with else
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
      if (capturesScope) {
        write(0);
      } else {
        write(lookupID++);
      }
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
    write(fn->body->capturesScope);
    if (fn->name->name != "_") {
      emitOpcode(Opcode::bind);
      write(INDEX_OF(m_ast->strings, fn->name->name));
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

    std::vector<unsigned> captured;
    for (unsigned i = 0; i < fn->arguments.size(); i++) {
      write(INDEX_OF(m_ast->strings, fn->arguments[i]->name));

      if (fn->arguments[i]->isCaptured) {
        captured.push_back(i);
      }
    }

    fn->body->prologue = [captured, this, fn]() {
      for (auto i : captured) {
        emitOpcode(Opcode::put_to_scope);
        write(INDEX_OF(m_ast->strings, fn->arguments[i]->name));
        write(i);
      }
    };

    capturesScope = fn->body->capturesScope;
    generateNode(fn->body);

    emitOpcode(Opcode::ret);
  }

  void Generator::generateIf(std::shared_ptr<AST::If> iff) {
    generateNode(iff->condition);

    bool hasElse = iff->elseBody != nullptr && iff->elseBody->nodes.size() > 0;
    emitJmp(Opcode::jz, iff->ifBody, hasElse);

    if (hasElse) {
      emitJmp(Opcode::jmp, iff->elseBody);
    }
  }

  void Generator::generateProgram(std::shared_ptr<AST::Program> program) {
    generateBlock(program->body);

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

    unsigned index = m_output.tellp();
    while (index++ % WORD_SIZE) {
      m_output.put(0);
    }

    if (functions.length()) {
      write(Section::Header);
      write(Section::Functions);
      m_output << functions;
    }

    write(Section::Header);
    write(Section::Text);
    write(lookupID);
    m_output << text;
  }

  void Generator::generateBlock(std::shared_ptr<AST::Block> block) {
    if (block->needsScope) {
      emitOpcode(Opcode::create_lex_scope);
    }

    if (block->prologue) {
      block->prologue();
    }

    for (auto node : block->nodes) {
      generateNode(node);
    }

    if (block->needsScope) {
      emitOpcode(Opcode::release_lex_scope);
    }
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
    <<  std::setfill(' ') << std::setw(width) << ((int)bytecode.tellg() - ((OFFSET) * WORD_SIZE)) \
    << "] " << padding << __VA_ARGS__ << "\n"

read_section:
    READ_INT(bytecode, ceos);
    assert(ceos == 0xCE05);

    READ_INT(bytecode, section);

    switch (section) {
      case Section::Strings:
        padding = "";
        WRITE(2, "STRINGS:");
        padding = "  ";
        goto section_strings;
        break;
      case Section::Functions: {
        padding = "";
        WRITE(2, "FUNCTIONS:");
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
        WRITE(2, "TEXT:");
        padding = "  ";
        goto section_code;
        break;
    }

section_strings:
    while (true) {
      READ_INT(bytecode, ceos);
      bytecode.seekg(-sizeof(ceos), bytecode.cur);
      if (ceos == Section::Header)  {
        goto read_section;
      }
      static unsigned str_index = 0;
      READ_STR(bytecode, str);
      while (bytecode.peek() == '\0') {
        bytecode.get();
      }
      strings.push_back(str);
      WRITE((float)(str.length() + 1)/WORD_SIZE, "$" << str_index++ << ": " << str);
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
      WRITE(3 + arg_count, strings[fn_id] << "(" << args.str() << "):");
      padding = "  ";

      while (true) {
        READ_INT(bytecode, header);
        bytecode.seekg(-sizeof(header), bytecode.cur);
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
    // skip size of lookup table
    bytecode.seekg(WORD_SIZE, bytecode.cur);
    while (true) {
      READ_INT(bytecode, opcode);
      if (bytecode.eof() || bytecode.fail()) {
        return;
      }
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
        WRITE(2, "push 0x" << std::setbase(16) << value << std::setbase(10));
        break;
      }
      case Opcode::call: {
        READ_INT(bytecode, nargs);
        WRITE(2, "call (" << nargs << ")");
        break;
      }
      case Opcode::load_string: {
        READ_INT(bytecode, stringID);
        WRITE(2, "load_string $" << stringID);
        break;
      }
      case Opcode::lookup: {
        READ_INT(bytecode, id);
        READ_INT(bytecode, cacheSlot);
        WRITE(3, "lookup $" << id << "(" << strings[id] << ") [cacheSlot=" << cacheSlot << "]");
        break;
      }
      case Opcode::create_closure: {
        READ_INT(bytecode, fnID);
        READ_INT(bytecode, capturesScope);
        WRITE(3, "create_closure " << functions[fnID] << " [capturesScope=" << (capturesScope ? "true" : "false") << "]");
        break;
      }
      case Opcode::jmp:  {
        READ_INT(bytecode, target);
        WRITE(2, "jmp [" << ((int)bytecode.tellg() - (2 * WORD_SIZE) + target) << "]");
        break;
      }
      case Opcode::jz: {
        READ_INT(bytecode, target);
        WRITE(2, "jz [" << ((int)bytecode.tellg() - (2 * WORD_SIZE) + target) << "]");
        break;
      }
      case Opcode::push_arg: {
        READ_INT(bytecode, arg);
        WRITE(2, "push_arg $" << arg);
        break;
      }
      case Opcode::put_to_scope: {
        READ_INT(bytecode, arg);
        READ_INT(bytecode, arg2);
        WRITE(3, "put_to_scope $" << strings[arg] << " = $" << arg2);
        break;
      }
      case Opcode::bind:
        READ_INT(bytecode, fnID);
        WRITE(2, "bind $" << functions[fnID]);
        break;
      case Opcode::ret: {
        WRITE(1, "ret");
        break;
      }
      case Opcode::create_lex_scope: {
        WRITE(1, "create_lex_scope");
        break;
      }
      case Opcode::release_lex_scope: {
        WRITE(1, "release_lex_scope");
        break;
      }
      case Opcode::exit:
        WRITE(1, "exit");
        break;
      default:
        std::cerr << "Unhandled opcode: " << Opcode::typeName(static_cast<Opcode::Type>(opcode)) << "\n";
        throw;
    }
  }
#undef WRITE
}
