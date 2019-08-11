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
    SymbolTable(SymbolTable* s);

    void put(const string& name, const Expr& val);
    optional<Expr> find(const string& name);
    void remove(const string& name);

    void dump(ostream& os);

private:
    map<string, Expr> table;
    SymbolTable* next;
};
}

#endif