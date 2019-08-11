//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "expr.hh"

#include <sstream>

#include "function.hh"

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
        auto l = any_cast<List>(s);
        if (l.empty()) {
            return os << "nil";
        }
        os << '(';
        for (auto i = l.begin(); i != l.end(); i++) {
            os << *i;
            if (i != l.end() - 1)
                os << ' ';
        }
        os << ')';
    } else if (s.type() == typeid(Function)) {
        os << string(any_cast<Function>(s));
    } else {
        os << "*Unprintable type*";
    }
    return os;
}

string to_string(const Expr& e)
{
    stringstream ss;
    ss << e;
    return ss.str();
}

Bool expr_eq(const Expr& x, const Expr& y)
{
    if (is_atomic(x) && is_atomic(y)) {
        if (is_a<Atom>(x) && is_a<Atom>(y) && any_cast<Atom>(x) == any_cast<Atom>(y)) {
            return sT;
        } else if (is_a<Int>(x) && is_a<Int>(y) && any_cast<Int>(x) == any_cast<Int>(y)) {
            return sT;
        } else if (is_a<Bool>(x) && is_a<Bool>(y) && any_cast<Bool>(x) == any_cast<Bool>(y)) {
            return sT;
        }
        return sF;
    }
    return sF;
}

Bool expr_eql(const Expr& x, const Expr& y)
{
    return expr_eq(x, y);
}

Bool expr_equal(const Expr& x, const Expr& y)
{
    if (expr_eql(x, y)) {
        return sT;
    }
    if (is_a<List>(x) && is_a<List>(y)) {
        auto xx = any_cast<List>(x);
        auto yy = any_cast<List>(y);
        if (xx.size() != yy.size()) {
            return sF;
        }
        auto itx = xx.begin();
        auto ity = yy.begin();
        for (; itx != xx.end(); itx++, ity++) {
            if (!expr_equal(*itx, *ity)) {
                return sF;
            }
        }
        return sT;
    }
    return sF;
}
}
