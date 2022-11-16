//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <iostream>
#include <map>
#include <string>

#include "expr.hh"

namespace ax {

class SymbolTable {
  public:
    SymbolTable(SymbolTable *const s) : next(s){};

    SymbolTable(const SymbolTable &) = delete; // stop copying

    inline void put(const std::string &name, const Expr val) { table[name] = val; };

    std::optional<Expr> find(const std::string &name) const;
    bool                set(const std::string &name, const Expr val);
    void                remove(const std::string &name);

    void dump(std::ostream &os) const;

  private:
    std::map<std::string, Expr> table;
    SymbolTable *const          next;
};

} // namespace ax
