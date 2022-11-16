//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "function.hh"

namespace ax {

Function_::operator std::string() {
    return "λ:" + name + " : " + to_string(body);
}

std::optional<Expr> get_keyword_value(const Expr &t, const Expr &k) {
    Expr top{t};
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

} // namespace ax