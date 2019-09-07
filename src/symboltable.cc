//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "symboltable.hh"

namespace ax {

optional<Expr*> SymbolTable::find(const string& name) const
{
    if (auto x = table.find(name); x != table.end()) {
        return x->second;
    }
    if (next) {
        return next->find(name);
    }
    return {};
}

bool SymbolTable::set(const string& name, Expr* const val)
{
    if (auto x = table.find(name); x != table.end()) {
        put(name, val);
        return true;
    } else {
        // not found, check above
        if (next) {
            return next->set(name, val);
        }
        return false;
    }
};

void SymbolTable::remove(const string& name)
{
    if (auto x = table.find(name); x != table.end()) {
        table.erase(name);
        return;
    } else if (next) {
        next->remove(name);
    }
}

void SymbolTable::dump(ostream& os) const
{
    os << "Dump symbol table: " << endl;
    for (auto x : table) {
        os << x.first << " -> " << to_string(x.second) << endl;
    }
    if (next) {
        next->dump(os);
    }
}
}
