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
#include "parser.hh"

namespace ax {

Expr* mk_list(initializer_list<Expr*> p)
{
    if (p.size() == 0) {
        return sF;
    }
    Expr* start = mk_list();
    Expr* l = start;
    for (auto iter = p.begin(); iter != p.end(); iter++) {
        l->car = *iter;
        if (iter != p.end() - 1) {
            l->cdr = mk_list();
            l = l->cdr;
        }
    }
    return start;
}

string to_dstring(const Expr* s)
{
    if (!s) {
        return "NULL";
    }
    switch (s->type) {
    case Type::boolean:
        return s->boolean ? "t" : "nil";
    case Type::atom:
        return s->atom;
    case Type::integer:
        return std::to_string(s->integer);
    case Type::list: {
        string res("[");
        res += to_dstring(s->car);
        res += " . ";
        res += to_dstring(s->cdr);
        res += "]";
        return res;
    }
    case Type::function:
        return string(*s->function);
    case Type::keyword:
        return s->keyword;
    default:
        return "<Unknown>";
    }
}

string to_string(const Expr* s)
{
    if (s == nullptr) {
        return "";
    }
    switch (s->type) {
    case Type::boolean:
        return s->boolean ? "t" : "nil";

    case Type::atom:
        return s->atom;
    case Type::integer:
        return std::to_string(s->integer);
        // } else if (s.type() == typeid(Float)) {
        //     array<char, 80> buf;
        //     sprintf(buf.data(), "%.12lg", any_cast<Float>(s));
        //     return string(buf.data());

    case Type::list: {
        if (s->car == nullptr && s->cdr == nullptr) {
            return "nil";
        }
        if (s->car == quote_at) {
            return "'" + to_string(s->cdr->car);
        }
        string str{ '(' };
        str += to_string(s->car);
        while (s->cdr != nullptr) {
            if (is_atomic(s->cdr)) {
                str += " . ";
                str += to_string(s->cdr);
                break;
            } else {
                s = s->cdr;
                if (s->car != nullptr) {
                    str += ' ';
                    str += to_string(s->car);
                }
            }
        };
        str += ')';
        return str;
    }
        // } else if (s.type() == typeid(String)) {
        //     return "\"" + ws2s(any_cast<String>(s)) + "\"";
        // } else if (s.type() == typeid(Char)) {
        //     string str("#\\");
        //     switch (any_cast<Char>(s)) {
        //     case ' ':
        //         str += "space";
        //         break;
        //     case '\n':
        //         str += "newline";
        //         break;
        //     default:
        //         utf8::append(any_cast<Char>(s), str);
        //     }
        //     return str;
    case Type::function:
        return string(*s->function);
    case Type::function_ref:
        return "#'" + s->function_ref;
    case Type::keyword:
        return s->keyword;
    default:
        return "*Unprintable type*";
    }
}

unsigned int size_list(const Expr* s)
{
    unsigned int res = 0;
    while (s && is_list(s)) {
        res++;
        s = s->cdr;
    }
    return res;
}

constexpr bool same_type(Type t, const Expr* x, const Expr* y)
{
    return t == x->type && x->type == y->type;
}

Expr* expr_eq(const Expr* x, const Expr* y)
{
    if (is_false(x) && is_false(y)) {
        return sT;
    }
    if (is_atomic(x) && is_atomic(y)) {
        if (same_type(Type::atom, x, y) && x->atom == y->atom) {
            return sT;
        } else if (same_type(Type::integer, x, y) && x->integer == y->integer) {
            return sT;
        } else if (same_type(Type::boolean, x, y) && x->boolean == y->boolean) {
            return sT;
            //} else if (eq_any<Char>(x, y)) {
            //    return sT;
            //} else if (eq_any<Float>(x, y)) {
            //    return sT;
        }
        return sF;
    }
    return sF;
}

Expr* expr_eql(const Expr* x, const Expr* y)
{
    return expr_eq(x, y);
}

Expr* expr_equal(const Expr* x, const Expr* y)
{
    if (expr_eql(x, y) == sT) {
        return sT;
    }
    if (same_type(Type::list, x, y)) {
        if (size_list(x) != size_list(y)) {
            return sF;
        }
        auto itx = x;
        auto ity = y;
        while (itx) {
            if (expr_equal(itx->car, ity->car) == sF) {
                return sF;
            }
            itx = itx->cdr;
            ity = ity->cdr;
        }
        return sT;
    }
    //if (eq_any<String>(x, y)) {
    //    return sT;
    //}
    return sF;
}

/*
Float as_Float(const Expr& s)
{
    if (is_a<Float>(s)) {
        return any_cast<Float>(s);
    } else if (is_a<Int>(s)) {
        return Float(any_cast<Int>(s));
    } else
        throw EvalException("Not number");
}

Expr make_type(const Atom& t, size_t size)
{
    if (t == type_atom) {
        return Atom{};
    } else if (t == type_list) {
        List l;
        for (int i = 0; i < size; i++) {
            l.push_back(sF);
        }
        return l;
    } else if (t == type_int) {
        return Int{ 0 };
    } else if (t == type_float) {
        return Float{ 0.0 };
    } else if (t == type_string) {
        return String(size, U' ');
    } else if (t == type_char) {
        return Char{ 0 };
    } else if (t == type_funct) {
        throw RuntimeException("Can't make empty function ref");
    } else if (t == type_bool) {
        return sT;
    } else if (t == type_null) {
        return sF;
    }
    throw EvalException("Unknown type: " + to_string(t));
}
*/
}
