//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef EXPR_HH
#define EXPR_HH

#include <codecvt>
#include <iostream>
#include <locale>
#include <string>

#include <gc_cpp.h>

namespace ax {

using namespace std;

enum class Type {
    atom,
    list
};

using Atom = string;

class List {
}; // Dummy type

struct Expr {
    Expr(Type t)
        : type(t){};
    Type type;
    union {
        Atom atom;
        struct {
            Expr* car;
            Expr* cdr;
        };
    };
};

inline Expr* mk_atom(const string& s)
{
    auto e = new (GC) Expr(Type::atom);
    e->atom = s;
    return e;
}

inline Expr* mk_list()
{
    auto e = new (GC) Expr(Type::list);
    e->car = e->cdr = nullptr;
    return e;
}

constexpr bool is_atom(const Expr* s)
{
    return s->type == Type::atom;
}

constexpr bool is_list(const Expr* s)
{
    return s->type == Type::list;
}

// Output

string to_string(const Expr* e);

// Bool

constexpr Expr* sF = nullptr;
inline const Expr* sT = mk_atom("t");

constexpr bool is_sF(const Expr* e)
{
    return e == nullptr;
}

constexpr bool is_false(const Expr* s)
// Is the Bool sF, or is the empty list
{
    return (s == nullptr)
        || (s->type == Type::list && s->car == nullptr);
}

/*
Bool expr_eq(const Expr& x, const Expr&);
Bool expr_eql(const Expr& x, const Expr&);
Bool expr_equal(const Expr& x, const Expr&);

class Keyword : public string {
public:
    Keyword(const string& s)
        : string(s){};
};

using Char = wchar_t;
using String = wstring;
*/

inline wstring s2ws(const std::string& str)
{
    std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> converterX;
    return converterX.from_bytes(str);
}

inline string ws2s(const std::wstring& wstr)
{
    std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> converterX;
    return converterX.to_bytes(wstr);
}

/*
using Float = double;

Float as_Float(const Expr& s);

constexpr bool is_Num(const Expr& n)
{
    return is_a<Int>(n) || is_a<Float>(n);
}

constexpr bool is_atomic(const Expr& s)
{
    return s.type() == typeid(Atom) || s.type() == typeid(Bool) || s.type() == typeid(Int) || s.type() == typeid(Char) || s.type() == typeid(Float);
}

// sequence

constexpr bool is_seq(const Expr& s)
{
    return is_a<List>(s) || is_a<String>(s);
}

// types

const Atom type_atom{ "symbol" };
const Atom type_list{ "cons" };
const Atom type_int{ "integer" };
const Atom type_float{ "float" };
const Atom type_string{ "string" };
const Atom type_char{ "char" };
const Atom type_funct{ "function" };
const Atom type_bool{ "boolean" };
const Atom type_null{ "null" };

Expr make_type(const Atom& t, size_t size = 0);

inline bool is_seq_type(const Atom& s)
{
    return s == type_list || s == type_string;
}
*/
}
#endif