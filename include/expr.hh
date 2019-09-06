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

// basic types for objects
enum class Type {
    atom,
    boolean,
    integer,
    floating,
    list,
    character,
    string,
    function,
    keyword,
    function_ref
};

// defintions of basic types
using Atom = string;
using Bool = bool;
using Int = long;
using Float = double;
using Keyword = string;
using Char = wchar_t;
using String = wstring;

class Function;

// Basic element in s-expressions.
class Expr {
public:
    Expr(Type t)
        : type(t){};

    Type type;

    // The element can hold the basic types, as a union,
    // This is ugly, but makes easier programming as compared to
    // C++17 templated solution.
    union {
        Atom atom;
        Bool boolean;
        Int integer;
        Float floating;
        struct {
            // THis makes easier access as unamed structure,
            // but is not standard C++.
            Expr* car;
            Expr* cdr;
        };
        Char chr;
        String string;
        Function* function;
        Keyword keyword;
        Atom function_ref;
    };

    // Return the size of the list, 0 if not list.
    unsigned int size() const noexcept;

    Expr* at(size_t pos);
    Expr* operator[](size_t pos) { return at(pos); };
    Expr* from(size_t pos);
    void set(size_t pos, Expr* r);
    Expr* find(Expr* r);
};

// Various constructors for element types, returning a s-expression

inline Expr* mk_atom(const Atom& s)
{
    auto e = new (GC) Expr(Type::atom);
    e->atom = s;
    return e;
}

inline Expr* mk_bool(const bool s)
{
    auto e = new Expr(Type::boolean); // bools are not make often and last forever.
    e->boolean = s;
    return e;
}

inline Expr* mk_int(const Int i)
{
    auto e = new (GC) Expr(Type::integer);
    e->integer = i;
    return e;
}

inline Expr* mk_float(const Float f)
{
    auto e = new (GC) Expr(Type::floating);
    e->floating = f;
    return e;
}

inline Expr* mk_list(Expr* car = nullptr, Expr* cdr = nullptr)
{
    auto e = new (GC) Expr(Type::list);
    e->car = car;
    e->cdr = cdr;
    return e;
}

Expr* mk_list(initializer_list<Expr*>);
Expr* mk_list(size_t size, Expr* const init);

// Test functions for s-expression types
template <Type t>
constexpr bool is_a(const Expr* s)
{
    return s->type == t;
};

constexpr bool is_atom(const Expr* s)
{
    return s->type == Type::atom;
}

constexpr bool is_bool(const Expr* s)
{
    return s->type == Type::boolean;
}

constexpr bool is_int(const Expr* s)
{
    return s->type == Type::integer;
}

constexpr bool is_list(const Expr* s)
{
    return s->type == Type::list;
}

constexpr bool is_atomic(const Expr* s)
{
    return s->type != Type::list;
}

// Working on lists of arguments
constexpr Expr* arg0(Expr* args)
{
    return args->car;
}

constexpr Expr* arg1(Expr* args)
{
    return args->cdr->car;
}

constexpr Expr* arg2(Expr* args)
{
    return args->cdr->cdr->car;
}

// Output

string to_string(const Expr* e);
string to_dstring(const Expr* e);

inline ostream& operator<<(ostream& os, const Expr* s)
{
    return os << to_string(s);
}

// Bool

inline Expr* const sF = mk_bool(false);
inline Expr* const sT = mk_bool(true);

inline bool is_sF(const Expr* e)
{
    return e == sF || e == nullptr;
}

inline bool is_false(const Expr* s)
// Is the Bool sF, or is the empty list
{
    return is_sF(s) || (s->type == Type::list && s->car == nullptr);
}

Expr* expr_eq(const Expr* x, const Expr*);
Expr* expr_eql(const Expr* x, const Expr*);
Expr* expr_equal(const Expr* x, const Expr*);

inline Expr* mk_char(Char c)
{
    auto e = new (GC) Expr(Type::character);
    e->chr = c;
    return e;
}

inline Expr* mk_string(String s)
{
    auto e = new (GC) Expr(Type::string);
    e->string = s;
    return e;
}

// string <-> wstring conversion functions

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

Float as_float(Expr* const s);

constexpr bool is_number(Expr* n)
{
    return is_a<Type::integer>(n) || is_a<Type::floating>(n);
}

// sequence

constexpr bool is_seq(const Expr* s)
{
    return is_a<Type::list>(s) || is_a<Type::string>(s);
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

inline bool is_seq_type(const Atom& s)
{
    return s == type_list || s == type_string;
}
}
#endif