#include <iostream>
#include <sstream>
#include <vector>
#include <unordered_map>

#ifndef CEOS_VM_H
#define CEOS_VM_H

namespace ceos {
  class VM;

  typedef uintptr_t (*JSFunctionType)(VM &, unsigned);

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
        registerBuiltins();
      }

      void registerBuiltins();
      void execute();
      void loadStrings();
      void loadFunctions();
      void run();

      void stack_push(uintptr_t value) {
        if (esp > stack.size() - 2) {
          std::cerr << "Stack overflow\n";
          throw;
        }
        stack[esp++] = value;
      }

      uintptr_t stack_pop() {
        return stack[--esp];
      }

      uintptr_t arg(unsigned index) {
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
      
      std::vector<uintptr_t> stack;
      uintptr_t ebp;
      uintptr_t esp;
      uintptr_t pc;
      size_t length;

    private:

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

      struct Scope {
        Scope(std::shared_ptr<Scope> p) : parent(p) { }

        Scope(std::shared_ptr<Scope> p, std::shared_ptr<Scope> o) : parent(p) , other(o) { }

        uintptr_t get(std::string &var) {
          uintptr_t v;
          if ((v = table[var])) return v;
          else if (parent) return parent->get(var);
          else return 0;
        }

        std::shared_ptr<Scope> parent;
        std::shared_ptr<Scope> other;
        std::unordered_map<std::string, uintptr_t> table;
      };

      struct Lambda {
        Function *fn;
        std::shared_ptr<Scope> scope;
      };

      uint8_t *m_bytecode;
      std::vector<std::string> m_stringTable;
      std::unordered_map<std::string, uintptr_t> m_functionTable;
      std::vector<Function> m_userFunctions;
      std::shared_ptr<Scope> m_scope;
  };
}

#endif
