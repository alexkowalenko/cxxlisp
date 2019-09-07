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

Expr* mk_list(size_t size, Expr* const init)
{
    Expr* prev = nullptr;
    auto top = mk_list();
    auto s = top;
    while (size > 0) {
        s->car = init;
        s->cdr = mk_list();
        prev = s;
        s = s->cdr;
        size--;
    }
    if (prev) {
        prev->cdr = nullptr;
    }
    return top;
}

string to_dstring(const Expr* const s)
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
    case Type::string:
    case Type::character:
    case Type::function_ref:
    case Type::function:
    case Type::keyword:
    case Type::floating:
        return to_string(s);
    default:
        return "<Unknown>";
    }
}

string to_string(const Expr* const s)
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
    case Type::floating:
        array<char, 30> buf;
        sprintf(buf.data(), "%.12lg", s->floating);
        return string(buf.data());
    case Type::list: {
        if (s->car == nullptr && s->cdr == nullptr) {
            return "nil";
        }
        if (s->car == quote_at) {
            return "'" + to_string(s->cdr->car);
        }
        string str{ '(' };
        str += to_string(s->car);
        auto x = s;
        while (x->cdr != nullptr) {
            if (is_atomic(x->cdr)) {
                str += " . ";
                str += to_string(x->cdr);
                break;
            } else {
                x = x->cdr;
                if (x->car != nullptr) {
                    str += ' ';
                    str += to_string(x->car);
                }
            }
        };
        str += ')';
        return str;
    }
    case Type::string:
        return "\"" + ws2s(s->string) + "\"";
    case Type::character: {
        string str("#\\");
        switch (s->chr) {
        case ' ':
            str += "space";
            break;
        case '\n':
            str += "newline";
            break;
        default:
            utf8::append(s->chr, str);
        }
        return str;
    }
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

unsigned int Expr::size() const noexcept
{
    unsigned int res = 0;
    for (auto s = this; s && is_list(s); res++, s = s->cdr) {
    }
    return res;
}

Expr* Expr::at(size_t pos) const noexcept
{
    for (auto s = this; !is_false(s); pos--, s = s->cdr) {
        if (!pos) {
            return s->car;
        }
    }
    return sF;
}

Expr* Expr::from(size_t pos) noexcept
{
    for (auto s = this; !is_false(s); pos--, s = s->cdr) {
        if (!pos) {
            return s;
        }
    }
    return sF;
}

void Expr::set(size_t pos, Expr* r) noexcept
{
    for (auto s = this; !is_false(s); pos--, s = s->cdr) {
        if (!pos) {
            s->car = r;
            return;
        }
    }
}

Expr* Expr::find(Expr* r) noexcept
{
    for (auto s = this; !is_false(s); s = s->cdr) {
        if (expr_eq(s->car, r)) {
            return s;
        }
    }
    return nullptr;
}

constexpr bool same_type(Type t, const Expr* x, const Expr* y)
{
    return t == x->type && x->type == y->type;
}

Expr* expr_eq(const Expr* const x, const Expr* const y)
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
        } else if (same_type(Type::character, x, y) && x->chr == y->chr) {
            return sT;
        } else if (same_type(Type::floating, x, y) && x->floating == y->floating) {
            return sT;
        } else if (same_type(Type::keyword, x, y) && x->keyword == y->keyword) {
            return sT;
        }
        return sF;
    }
    return sF;
}

Expr* expr_eql(const Expr* const x, const Expr* const y)
{
    return expr_eq(x, y);
}

Expr* expr_equal(const Expr* const x, const Expr* const y)
{
    if (expr_eql(x, y) == sT) {
        return sT;
    }
    if (same_type(Type::list, x, y)) {
        if (x->size() != y->size()) {
            return sF;
        }
        auto itx = x;
        auto ity = y;
        while (itx) {
            if (same_type(Type::list, itx, ity)) {
                if (expr_equal(itx->car, ity->car) == sF) {
                    return sF;
                }
                itx = itx->cdr;
                ity = ity->cdr;
            } else {
                return expr_equal(itx, ity);
            }
        }
        return sT;
    } else if (same_type(Type::string, x, y) && x->string == y->string) {
        return sT;
    }
    return sF;
}

Float as_float(const Expr* const s)
{
    if (is_a<Type::floating>(s)) {
        return s->floating;
    } else if (is_a<Type::integer>(s)) {
        return Float(s->integer);
    } else
        throw EvalException("Not number");
}
}
