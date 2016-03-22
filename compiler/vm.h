#include <iostream>
#include <sstream>
#include <vector>
#include <unordered_map>

#include "value.h"

#ifndef CEOS_VM_H
#define CEOS_VM_H

namespace ceos {
  struct Scope {
    Scope(std::shared_ptr<Scope> p) : parent(p) { }

    Scope(std::shared_ptr<Scope> p, std::shared_ptr<Scope> o) : parent(p) , other(o) { }

    Value get(std::string &var) {
      std::unordered_map<std::string, Value>::iterator v;
      if ((v = table.find(var)) != table.end()) return v->second;
      else if (parent.get() != this) return parent->get(var);
      else {
        std::cerr << "Symbol not found: " << var << "\n";
        throw;
      }
    }

    std::shared_ptr<Scope> parent;
    std::shared_ptr<Scope> other;
    std::unordered_map<std::string, Value> table;
  };

  class VM {
    public:
      VM(std::stringstream &bs):
        stack(2048),
        pc(0) {
        std::string bc = bs.str();
        length = bc.length();
        m_bytecode = (uint8_t *)malloc(length);
        memcpy(m_bytecode, bc.data(), length);
        m_scope = std::make_shared<Scope>(nullptr);
        m_scope->parent = m_scope; // global scope
        registerBuiltins();
      }

      void registerBuiltins();
      void execute();
      void loadStrings();
      void loadFunctions();
      void run();

      void stack_push(Value value) {
        if (esp > stack.size() - 2) {
          std::cerr << "Stack overflow\n";
          throw;
        }
        stack[esp++] = value;
      }

      Value stack_pop() {
        return stack[--esp];
      }

      Value arg(unsigned index) {
        return stack[ebp - index - 4];
      }

      template<typename T>
      T read() {
        T v = *(T *)(m_bytecode + pc);
        pc += sizeof(T);
        return v;
      }

      char *readStr() {
        char *v = (char *)(m_bytecode + pc);
        pc += strlen(v) + 1;
        return v;
      }

      std::vector<Value> stack;
      unsigned ebp;
      unsigned esp;
      unsigned pc;
      size_t length;

      std::vector<std::string> m_stringTable;
    private:


      uint8_t *m_bytecode;
      std::vector<Function> m_userFunctions;
      std::shared_ptr<Scope> m_scope;
  };

  struct Function {
    Function(unsigned i, unsigned args, unsigned o, std::vector<std::string *> &&a) : offset(o), id(i), nargs(args), args(a) {}

    std::string &name(VM *vm) {
      return vm->m_stringTable[id];
    }

    std::string &arg(unsigned i) {
      return *args[i];
    }

    unsigned offset;
    unsigned id;
    unsigned nargs;
    std::vector<std::string *> args;
  };


  struct Closure {
    Function *fn;
    std::shared_ptr<Scope> scope;
  };
}

#endif
