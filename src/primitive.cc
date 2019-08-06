//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <vector>

namespace ax {

map<string, Primitive> prim_table;

Expr atom(List& args)
{
    if (is_atomic(args[0]) || is_false(args[0])) {
        return sT;
    }
    return sF;
}

Expr symbolp(List& args)
{
    auto a = args.front();
    if (is_atomic(a)
        && (a.type() == typeid(Atom) || a.type() == typeid(Bool))) {
        return sT;
    }
    if (a.type() == typeid(List) && as_a<List>(a).size() == 0) {
        return sT;
    }
    return sF;
}

void init_prims()
{
    vector<Primitive> defs{
        { "atom", &atom, one_arg },
        { "symbolp", &symbolp, one_arg }
    };

    for (auto p : defs) {
        prim_table[p.name] = p;
    }
}
}
