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
#include <vector>

#include <any>

namespace ax {

using namespace std;

using Expr = any;

using Atom = string;
using Bool = bool;
using Int = long;
using List = vector<any>;

template <typename T>
constexpr bool is_a(const Expr& s)
{
    return s.type() == typeid(T);
}

// Output

string to_string(const Expr& e);

// Bool

constexpr Bool sF = Bool{ false };
constexpr Bool sT = Bool{ true };

constexpr bool is_sF(const Expr& e)
{
    return is_a<Bool>(e) && !any_cast<Bool>(e);
}

constexpr bool is_false(const Expr& s)
// Is the Bool sF, or is the empty list
{
    return (s.type() == typeid(Bool) && !any_cast<Bool>(s))
        || (s.type() == typeid(List) && any_cast<List>(s).empty());
}

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

constexpr bool is_Seq(const Expr& s)
{
    return is_a<List>(s) || is_a<String>(s);
}

} // namespace ax

#endif