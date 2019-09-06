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
    SymbolTable(SymbolTable* s)
        : next(s){};

    SymbolTable(const SymbolTable&) = delete; // stop copying

    inline void put(const string& name, Expr* const val)
    {
        table[name] = val;
    };

    optional<Expr*> find(const string& name);
    bool set(const string& name, Expr* const val);
    void remove(const string& name);

    void dump(ostream& os);

private:
    map<string, Expr*> table;
    SymbolTable* next;
};
}

#endif