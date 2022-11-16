//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <codecvt>
#include <complex>
#include <cstddef>
#include <iostream>
#include <locale>
#include <memory>
#include <string>
#include <variant>
#include <vector>

// #pragma clang diagnostic push
// #pragma clang diagnostic ignored "-Wshadow"
// #pragma clang diagnostic ignored "-Wconversion"
// #pragma clang diagnostic ignored "-Wold-style-cast"
// #pragma clang diagnostic ignored "-Wunused-parameter"
// #pragma clang diagnostic ignored "-Wcast-align"
// #include <gc_cpp.h>
// #pragma clang diagnostic pop

namespace ax {

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

class Expr_;
using Expr = std::shared_ptr<Expr_>;

// defintions of basic types
using Atom = std::string;
using Bool = bool;
using Int = long;
using Float = double;
using Keyword = std::string;
using Char = wchar_t;
using String = std::wstring;
using Vector = std::vector<Expr>;
using Complex = std::complex<Float>;

class Function_;
using Function = std::shared_ptr<Function_>;
class Stream;

// Basic element in s-expressions.
class Expr_ : public std::enable_shared_from_this<Expr_> {
  public:
    Expr_(Type t) : type(t){};
    ~Expr_(){};

    Type type;

    // The element can hold the basic types, as a union,
    // This is ugly, but makes easier programming as compared to
    // C++17 templated solution.
    union {
        Atom    atom;
        Bool    boolean;
        Int     integer;
        Float   floating;
        Complex complex;

#pragma clang diagnostic ignored "-Wgnu-anonymous-struct"
#pragma clang diagnostic ignored "-Wnested-anon-types"
        struct {
            // THis makes easier access as unamed structure,
            // but is not standard C++.
            Expr car;
            Expr cdr;
        };
        Char                    chr;
        String                  string;
        Function                function;
        Keyword                 keyword;
        Atom                    function_ref;
        std::shared_ptr<Stream> stream;
        Vector                  vector;
    };

    // Return the size of the list, 0 if not list.
    size_t size() noexcept;

    Expr at(size_t pos) noexcept;
    Expr operator[](size_t pos) noexcept { return at(pos); };
    Expr from(size_t pos) noexcept;
    void set(size_t pos, Expr r) noexcept;
    Expr find(Expr r) noexcept;
};

// Various constructors for element types, returning a s-expression

inline Expr mk_atom(const Atom &s) {
    auto e = std::make_shared<Expr_>(Type::atom);
    e->atom = s;
    return e;
}

inline Expr mk_bool(const bool s) {
    auto e = std::make_shared<Expr_>(Type::boolean); // bools are not make often and last forever.
    e->boolean = s;
    return e;
}

inline Expr mk_int(const Int i) {
    auto e = std::make_shared<Expr_>(Type::integer);
    e->integer = i;
    return e;
}

inline Expr mk_float(const Float f) {
    auto e = std::make_shared<Expr_>(Type::floating);
    e->floating = f;
    return e;
}

inline Expr mk_complex(const Complex c) {
    auto e = std::make_shared<Expr_>(Type::complex);
    e->complex = c;
    return e;
}

inline Expr mk_list(Expr car = nullptr, Expr cdr = nullptr) {
    auto e = std::make_shared<Expr_>(Type::list);
    e->car = car;
    e->cdr = cdr;
    return e;
}

Expr mk_list(std::initializer_list<Expr>);
Expr mk_list(size_t size, const Expr &init);

// Test functions for s-expression types
template <Type t> inline bool is_a(const Expr &s) {
    return s->type == t;
};

inline bool is_atom(const Expr &s) {
    return s->type == Type::atom;
}

inline bool is_bool(const Expr &s) {
    return s->type == Type::boolean;
}

inline bool is_int(const Expr &s) {
    return s->type == Type::integer;
}

inline bool is_list(const Expr &s) {
    return s->type == Type::list;
}

inline bool is_atomic(const Expr &s) {
    return s->type != Type::list;
}

// Working on lists of arguments
inline Expr arg0(const Expr &args) {
    return args->car;
}

inline Expr arg1(const Expr &args) {
    return args->cdr->car;
}

inline Expr arg2(const Expr &args) {
    return args->cdr->cdr->car;
}

// Output

std::string to_string(const Expr &e);
std::string to_dstring(const Expr &e);
std::string to_pstring(const Expr &s);

inline std::ostream &operator<<(std::ostream &os, const Expr &s) {
    return os << to_string(s);
}

// Bool

inline const Expr sF = mk_bool(false);
inline const Expr sT = mk_bool(true);

inline bool is_sF(const Expr &e) {
    return e == sF || e == nullptr;
}

inline bool is_false(const Expr &s)
// Is the Bool sF, or is the empty list
{
    return is_sF(s) || (s->type == Type::list && s->car == nullptr);
}

Expr expr_eq(const Expr &x, const Expr &y);
Expr expr_eql(const Expr &x, const Expr &y);
Expr expr_equal(const Expr &x, const Expr &);

inline Expr mk_char(Char c) {
    auto e = std::make_shared<Expr_>(Type::character);
    e->chr = c;
    return e;
}

inline Expr mk_string(const String &s) {
    auto e = std::make_shared<Expr_>(Type::string);
    e->string = s;
    return e;
}

// string <-> wstring conversion functions

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"

inline std::wstring s2ws(const std::string &str) {
    std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> converterX;
    return converterX.from_bytes(str);
}

inline std::string ws2s(const std::wstring &wstr) {
    std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> converterX;
    return converterX.to_bytes(wstr);
}

#pragma clang diagnostic pop

Float as_float(const Expr &s);

inline bool is_number(const Expr &n) {
    return is_a<Type::integer>(n) || is_a<Type::floating>(n);
}

// sequence

inline bool is_seq(const Expr &s) {
    return is_a<Type::list>(s) || is_a<Type::string>(s);
}

// types

const Atom type_atom{"symbol"};
const Atom type_list{"cons"};
const Atom type_list2{"list"};
const Atom type_int{"integer"};
const Atom type_float{"float"};
const Atom type_complex{"complex"};
const Atom type_string{"string"};
const Atom type_char{"char"};
const Atom type_funct{"function"};
const Atom type_bool{"boolean"};
const Atom type_null{"null"};
const Atom type_vector{"vector"};

inline bool is_seq_type(const Atom &s) {
    return s == type_list || s == type_list2 || s == type_string;
}

inline Expr mk_stream(std::shared_ptr<Stream> s) {
    auto e = std::make_shared<Expr_>(Type::stream);
    e->stream = s;
    return e;
}

enum class StreamType { input, output };

class Stream {
  public:
    Stream(std::istream *s) : stream_type(StreamType::input), str(s){};

    Stream(std::ostream *s) : stream_type(StreamType::output), str(s){};

    Stream(){};

    std::string to_string();
    bool        is_input() { return stream_type == StreamType::input; };
    bool        is_output() { return stream_type == StreamType::output; };

    StreamType                                                   stream_type;
    std::variant<std::istream *, std::ostream *, std::fstream *> str;
};

inline Expr mk_stream(std::istream *s) {
    auto e = std::make_shared<Expr_>(Type::stream);
    e->stream = std::make_shared<Stream>(s);
    return e;
}

inline Expr mk_stream(std::ostream *s) {
    auto e = std::make_shared<Expr_>(Type::stream);
    e->stream = std::make_shared<Stream>(s);
    return e;
}

Expr mk_stream(std::fstream *const s, std::ios_base::openmode m);

// Vectors
inline Expr mk_vector() {
    auto v = std::make_shared<Expr_>(Type::vector);
    v->vector = Vector();
    return v;
}

Expr to_vector(Expr);
} // namespace ax