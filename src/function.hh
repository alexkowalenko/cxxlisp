//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include "expr.hh"

#include <optional>

namespace ax {

class Function {
  public:
    Function(Atom n, Expr *p) : name(n), parameters(p){};

    operator std::string();

    Atom  name;
    Expr *parameters;
    Expr *body;
    bool  macro = false;
};

inline Expr *mk_function(Function *f) {
    auto e = new (GC) Expr(Type::function);
    e->function = f;
    return e;
}

inline Expr *mk_keyword(const std::string &k) {
    auto e = new (GC) Expr(Type::keyword);
    e->keyword = k;
    return e;
}

inline Expr *mk_function_ref(const std::string &k) {
    auto e = new (GC) Expr(Type::function_ref);
    e->function_ref = k;
    return e;
}

std::optional<Expr *> get_keyword_value(Expr *const args, const Expr *k);

} // namespace ax
