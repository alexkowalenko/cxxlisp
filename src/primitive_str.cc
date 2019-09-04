//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <boost/algorithm/string.hpp>

namespace ax {

//
// String functions
//

PrimBasicFunct funct_ci(PrimBasicFunct f, function<Expr*(const Expr*)> trans)
{
    return [=](Expr* args) -> Expr* {
        auto new_list = mk_list();
        auto nl = new_list;
        Expr* prev = nullptr;
        while (!is_false(args)) {
            nl->car = trans(args->car);
            nl->cdr = mk_list();
            prev = nl;
            nl = nl->cdr;
            args = args->cdr;
        }
        prev->cdr = nullptr;
        return f(new_list);
    };
}

Expr* string_fnct(const string& name, Expr* args)
{
    wstring s;
    if (is_a<Type::string>(args->car)) {
        s = args->car->string;
    } else if (is_a<Type::character>(args->car)) {
        s += args->car->chr;
    } else {
        s = s2ws(to_string(args->car));
    }
    if (name == "string-upcase") {
        return mk_string(boost::algorithm::to_upper_copy(s));
    } else if (name == "string-downcase") {
        return mk_string(boost::algorithm::to_lower_copy(s));
    }
    return mk_string(s);
}
}