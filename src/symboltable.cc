//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "symboltable.hh"

namespace ax {

SymbolTable::SymbolTable(SymbolTable* s)
    : next(s){};

void SymbolTable::put(const string& name, const Expr& val)
{
    table[name] = val;
};

optional<Expr> SymbolTable::find(const string& name)
{
    if (auto x = table.find(name); x != table.end()) {
        return x->second;
    }
    if (next) {
        return next->find(name);
    }
    return {};
}

void SymbolTable::remove(const string& name)
{
    if (auto x = table.find(name); x != table.end()) {
        table.erase(name);
        return;
    } else if (next) {
        next->remove(name);
    }
}

void SymbolTable::dump(ostream& os)
{
    os << "Dump symbol table: " << endl;
    for (auto x : table) {
        os << x.first << " -> " << x.second << endl;
    }
    if (next) {
        next->dump(os);
    }
}
}
