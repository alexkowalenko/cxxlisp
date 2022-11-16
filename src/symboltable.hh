//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <string>

#include "expr.hh"

namespace ax {

class SymbolTable_;
using SymbolTable = std::shared_ptr<SymbolTable_>;

class SymbolTable_ {
  public:
    SymbolTable_(SymbolTable s) : next(s){};

    SymbolTable_(const SymbolTable_ &) = delete; // stop copying

    inline void put(const std::string &name, const Expr val) { table[name] = val; };

    std::optional<Expr> find(const std::string &name) const;
    bool                set(const std::string &name, const Expr val);
    void                remove(const std::string &name);

    void dump(std::ostream &os) const;

  private:
    std::map<std::string, Expr> table;
    SymbolTable                 next;
};

inline SymbolTable mk_symbol_table(SymbolTable s = nullptr) {
    return std::make_shared<SymbolTable_>(s);
}

} // namespace ax
