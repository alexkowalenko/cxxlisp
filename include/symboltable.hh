//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef SYMBOLTABLE_HH
#define SYMBOLTABLE_HH

#include <iostream>
#include <map>
#include <string>

#include "expr.hh"

namespace ax {

using namespace std;

class SymbolTable {
public:
    SymbolTable(SymbolTable* const s)
        : next(s){};

    SymbolTable(const SymbolTable&) = delete; // stop copying

    inline void put(const string& name, Expr* const val)
    {
        table[name] = val;
    };

    optional<Expr*> find(const string& name) const;
    bool set(const string& name, Expr* const val);
    void remove(const string& name);

    void dump(ostream& os) const;

private:
    map<string, Expr*> table;
    SymbolTable* const next;
};
}

#endif