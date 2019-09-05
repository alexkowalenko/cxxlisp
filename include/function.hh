//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef FUNCTION_HH
#define FUNCTION_HH

#include "expr.hh"

namespace ax {

class Function {
public:
    Function(Atom n, Expr* p)
        : name(n)
        , parameters(p){};

    operator string();

    Atom name;
    Expr* parameters;
    Expr* body;
    bool macro = false;
};

inline Expr* mk_function(Function* f)
{
    auto e = new (GC) Expr(Type::function);
    e->function = f;
    return e;
}

inline Expr* mk_keyword(const string& k)
{
    auto e = new (GC) Expr(Type::keyword);
    e->keyword = k;
    return e;
}

inline Expr* mk_function_ref(const string& k)
{
    auto e = new (GC) Expr(Type::function_ref);
    e->function_ref = k;
    return e;
}

Expr* get_keyword_value(Expr* const args, const Expr* k);
}

#endif