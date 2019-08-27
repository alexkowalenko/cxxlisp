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

/*
PrimBasicFunct funct_ci(PrimBasicFunct f, function<Expr(const Expr&)> trans)
{
    return [=](List& args) -> Expr {
        List nargs;
        for (auto x : args) {
            nargs.push_back(trans(x));
        }
        return f(nargs);
    };
}

Expr string_fnct(const string& name, List& args)
{
    wstring s;
    if (is_a<String>(args[0])) {
        s = any_cast<String>(args[0]);
    } else if (is_a<Char>(args[0])) {
        s += any_cast<Char>(args[0]);
    } else {
        s = s2ws(to_string(args[0]));
    }
    if (name == "string-upcase") {
        return String(boost::algorithm::to_upper_copy(s));
    } else if (name == "string-downcase") {
        return String(boost::algorithm::to_lower_copy(s));
    }
    return String(s);
}
*/
}