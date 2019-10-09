//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef EXPR_HH
#define EXPR_HH

#include <codecvt>
#include <complex>
#include <iostream>
#include <locale>
#include <string>
#include <variant>
#include <vector>

#include <gc_cpp.h>

namespace ax {

using namespace std;

// basic types for objects
enum class Type {
    atom,
    boolean,
    integer,
    floating,
    complex,
    list,
    character,
    string,
    function,
    keyword,
    function_ref,
    stream,
    vector
};

class Expr;

// defintions of basic types
using Atom = string;
using Bool = bool;
using Int = long;
using Float = double;
using Keyword = string;
using Char = wchar_t;
using String = wstring;
using Vector = vector<Expr*>;
using Complex = complex<Float>;

class Function;
class Stream;

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
        Complex complex;
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
        Stream* stream;
        Vector vector;
    };

    // Return the size of the list, 0 if not list.
    unsigned int size() const noexcept;

    Expr* at(size_t pos) const noexcept;
    Expr* operator[](size_t pos) const noexcept { return at(pos); };
    Expr* from(size_t pos) noexcept;
    void set(size_t pos, Expr* r) noexcept;
    Expr* find(Expr* r) noexcept;
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

inline Expr* mk_complex(const Complex c)
{
    auto e = new (GC) Expr(Type::complex);
    e->complex = c;
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
constexpr bool is_a(const Expr* const s)
{
    return s->type == t;
};

constexpr bool is_atom(const Expr* const s)
{
    return s->type == Type::atom;
}

constexpr bool is_bool(const Expr* const s)
{
    return s->type == Type::boolean;
}

constexpr bool is_int(const Expr* const s)
{
    return s->type == Type::integer;
}

constexpr bool is_list(const Expr* const s)
{
    return s->type == Type::list;
}

constexpr bool is_atomic(const Expr* const s)
{
    return s->type != Type::list;
}

// Working on lists of arguments
constexpr Expr* arg0(const Expr* const args)
{
    return args->car;
}

constexpr Expr* arg1(const Expr* const args)
{
    return args->cdr->car;
}

constexpr Expr* arg2(const Expr* const args)
{
    return args->cdr->cdr->car;
}

// Output

string to_string(const Expr* const e);
string to_dstring(const Expr* const e);
string to_pstring(const Expr* const s);

inline ostream& operator<<(ostream& os, const Expr* const s)
{
    return os << to_string(s);
}

// Bool

inline Expr* const sF = mk_bool(false);
inline Expr* const sT = mk_bool(true);

inline bool is_sF(const Expr* const e)
{
    return e == sF || e == nullptr;
}

inline bool is_false(const Expr* const s)
// Is the Bool sF, or is the empty list
{
    return is_sF(s) || (s->type == Type::list && s->car == nullptr);
}

Expr* expr_eq(const Expr* const x, const Expr* const y);
Expr* expr_eql(const Expr* const x, const Expr* const y);
Expr* expr_equal(const Expr* const x, const Expr* const);

inline Expr* mk_char(Char c)
{
    auto e = new (GC) Expr(Type::character);
    e->chr = c;
    return e;
}

inline Expr* mk_string(const String& s)
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

Float as_float(const Expr* const s);

constexpr bool is_number(const Expr* const n)
{
    return is_a<Type::integer>(n) || is_a<Type::floating>(n);
}

// sequence

constexpr bool is_seq(const Expr* const s)
{
    return is_a<Type::list>(s) || is_a<Type::string>(s);
}

// types

const Atom type_atom{ "symbol" };
const Atom type_list{ "cons" };
const Atom type_list2{ "list" };
const Atom type_int{ "integer" };
const Atom type_float{ "float" };
const Atom type_complex{ "complex" };
const Atom type_string{ "string" };
const Atom type_char{ "char" };
const Atom type_funct{ "function" };
const Atom type_bool{ "boolean" };
const Atom type_null{ "null" };
const Atom type_vector{ "vector" };

inline bool is_seq_type(const Atom& s)
{
    return s == type_list || s == type_list2 || s == type_string;
}

inline Expr* mk_stream(Stream* s)
{
    auto e = new (GC) Expr(Type::stream);
    e->stream = s;
    return e;
}

enum class StreamType {
    input,
    output
};

class Stream {
public:
    Stream(istream* s)
        : stream_type(StreamType::input)
        , str(s){};

    Stream(ostream* s)
        : stream_type(StreamType::output)
        , str(s){};

    Stream(){};

    string to_string();
    bool is_input() { return stream_type == StreamType::input; };
    bool is_output() { return stream_type == StreamType::output; };

    StreamType stream_type;
    variant<istream*, ostream*, fstream*> str;
};

inline Expr* mk_stream(istream* s)
{
    auto e = new (GC) Expr(Type::stream);
    e->stream = new (GC) Stream(s);
    return e;
}

inline Expr* mk_stream(ostream* s)
{
    auto e = new (GC) Expr(Type::stream);
    e->stream = new (GC) Stream(s);
    return e;
}

Expr* mk_stream(fstream* const s, ios_base::openmode m);

// Vectors
inline Expr* mk_vector()
{
    auto v = new (GC) Expr(Type::vector);
    v->vector = Vector();
    return v;
}

Expr* to_vector(Expr*);
}
#endif