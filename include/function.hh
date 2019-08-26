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
    Function(Atom n, List p)
        : name(n)
        , parameters(p){};

    operator string();

    Atom name;
    List parameters;
    List body;
    bool macro = false;
};

class FunctionRef : public string {
public:
    FunctionRef(const string& s)
        : string(s){};
};

bool has_keyword(const List& args, const Keyword& k);
Expr get_keyword_value(const List& args, const Keyword& k);
}

#endif