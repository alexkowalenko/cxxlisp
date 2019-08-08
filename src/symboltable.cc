//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "symboltable.hh"

namespace ax {

void SymbolTable::put(const string& name, const Expr& val)
{
    table[name] = val;
};

optional<Expr> SymbolTable::find(const string& name)
{
    if (auto x = table.find(name); x != table.end()) {
        return x->second;
    }
    return {};
}

void SymbolTable::dump(ostream& os)
{
    os << "Dump symbol table: " << endl;
    for (auto x : table) {
        os << x.first << " -> " << x.second << endl;
    }
}
}
