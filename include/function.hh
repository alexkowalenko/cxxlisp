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

/*
class FunctionRef : public string {
public:
    FunctionRef(const string& s)
        : string(s){};
};

bool has_keyword(const List& args, const Keyword& k);
Expr get_keyword_value(const List& args, const Keyword& k);
*/
}

#endif