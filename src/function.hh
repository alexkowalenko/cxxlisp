//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include "expr.hh"

#include <optional>

namespace ax {

class Function_ {
  public:
    Function_(const Atom &n, const Expr &p) : name(n), parameters(p){};

    operator std::string();

    Atom name;
    Expr parameters;
    Expr body;
    bool macro{false};
};

using Function = std::shared_ptr<Function_>;

inline Function mk_function(const Atom &n, Expr &p) {
    return std::make_shared<Function_>(n, p);
}

inline Expr mk_function(const Function &f) {
    auto e = std::make_shared<Expr_>(Type::function);
    e->function = f;
    return e;
}

inline Expr mk_keyword(const std::string &k) {
    auto e = std::make_shared<Expr_>(Type::keyword);
    e->keyword = k;
    return e;
}

inline Expr mk_function_ref(const std::string &k) {
    auto e = std::make_shared<Expr_>(Type::function_ref);
    e->function_ref = k;
    return e;
}

std::optional<Expr> get_keyword_value(const Expr &args, const Expr &k);

} // namespace ax
