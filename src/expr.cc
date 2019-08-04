//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "expr.hh"

namespace ax {

ostream& operator<<(ostream& os, const Expr& s)
{
    if (s.type() == typeid(Atom)) {
        os << any_cast<Atom>(s);
    } else if (s.type() == typeid(Int)) {
        os << any_cast<Int>(s);
    } else if (s.type() == typeid(Bool)) {
        os << (any_cast<Bool>(s) ? "t" : "nil");
    } else if (s.type() == typeid(List)) {
        os << '(';
        auto l = any_cast<List>(s);
        size_t i = 0;
        for (auto x : l) {
            os << x;
            if (i != l.size() - 1)
                os << ' ';
            ++i;
        }
        os << ')';
    } else {
        os << "Unprintable type";
    }
    return os;
}
}
