//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "function.hh"

namespace ax {

using namespace std;

Function::operator string()
{
    return "λ:"s + name + " : " + to_string(body);
}

optional<Expr*> get_keyword_value(Expr* top, const Expr* k)
{
    while (!is_false(top)) {
        if (expr_eq(top->car, k) != sF) {
            if (!is_false(top->cdr)) {
                return arg1(top);
            }
        }
        top = top->cdr;
    }
    return {};
}
}