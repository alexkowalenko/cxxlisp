//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "expr.hh"

#include <array>
#include <sstream>

#include <stdio.h>

#include <utf8.h>

#include "exceptions.hh"
#include "function.hh"

namespace ax {

ostream& operator<<(ostream& os, const Expr& s)
{
    return os << to_string(s);
}

string to_string(const Expr& s)
{
    if (s.type() == typeid(Atom)) {
        return any_cast<Atom>(s);
    } else if (s.type() == typeid(Int)) {
        return std::to_string(any_cast<Int>(s));
    } else if (s.type() == typeid(Float)) {
        array<char, 80> buf;
        sprintf(buf.data(), "%.12lg", any_cast<Float>(s));
        return string(buf.data());
    } else if (s.type() == typeid(Bool)) {
        return any_cast<Bool>(s) ? "t" : "nil";
    } else if (s.type() == typeid(List)) {
        auto l = any_cast<List>(s);
        if (l.empty()) {
            return "nil";
        }
        string str;
        str += '(';
        for (auto i = l.begin(); i != l.end(); i++) {
            str += to_string(*i);
            if (i != l.end() - 1)
                str += ' ';
        }
        str += ')';
        return str;
    } else if (s.type() == typeid(String)) {
        return "\"" + ws2s(any_cast<String>(s)) + "\"";
    } else if (s.type() == typeid(Char)) {
        string str("#\\");
        switch (any_cast<Char>(s)) {
        case ' ':
            str += "space";
            break;
        case '\n':
            str += "newline";
            break;
        default:
            utf8::append(any_cast<Char>(s), str);
        }
        return str;
    } else if (s.type() == typeid(Function)) {
        return any_cast<Function>(s);
    } else if (s.type() == typeid(FunctionRef)) {
        return "#'" + any_cast<FunctionRef>(s);
    } else if (s.type() == typeid(Keyword)) {
        return any_cast<Keyword>(s);
    } else {
        return "*Unprintable type*";
    }
}

template <typename T>
constexpr bool same_type(const Expr& x, const Expr& y)
{
    return is_a<T>(x) && is_a<T>(y);
}

template <typename T>
constexpr bool eq_any(const Expr& x, const Expr& y)
{
    return same_type<T>(x, y) && any_cast<T>(x) == any_cast<T>(y);
}

Bool expr_eq(const Expr& x, const Expr& y)
{
    if (is_atomic(x) && is_atomic(y)) {
        if (eq_any<Atom>(x, y)) {
            return sT;
        } else if (eq_any<Int>(x, y)) {
            return sT;
        } else if (eq_any<Bool>(x, y)) {
            return sT;
        } else if (eq_any<Char>(x, y)) {
            return sT;
        } else if (eq_any<Float>(x, y)) {
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
    if (same_type<List>(x, y)) {
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
    if (eq_any<String>(x, y)) {
        return sT;
    }
    return sF;
}

Float as_Float(const Expr& s)
{
    if (is_a<Float>(s)) {
        return any_cast<Float>(s);
    } else if (is_a<Int>(s)) {
        return Float(any_cast<Int>(s));
    } else
        throw EvalException("Not number");
}
}
