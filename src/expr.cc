//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "expr.hh"

namespace ax {

using boost::any_cast;

bool is_nullptr(const Expr& x)
{
    return x.type() == typeid(nullptr_t);
};

bool is_Atom(const Expr& s)
{
    return s.type() == typeid(Atom);
};

bool is_Int(const Expr& s)
{
    return s.type() == typeid(Int);
};

bool is_List(const Expr& s)
{
    return s.type() == typeid(List);
};

List& as_List(Expr& s)
{
    return any_cast<List&>(s);
}

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
