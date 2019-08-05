//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef EXPR_HH
#define EXPR_HH

#include <boost/any.hpp>
#include <iostream>
#include <list>
#include <string>

namespace ax {
using namespace std;

using Expr = boost::any;
using boost::any_cast;

using Atom = string;
using Bool = bool;
using Int = long;
using List = list<boost::any>;

template <typename T>
constexpr bool is_a(const Expr& s)
{
    return s.type() == typeid(T);
}

template <typename T>
constexpr T& as_a(Expr& s)
{
    return any_cast<T&>(s);
}

// Output

ostream& operator<<(ostream& os, const Expr& e);
string to_string(const Expr& e);

// Bool

constexpr Bool sF = Bool{ false };
constexpr Bool sT = Bool{ true };

} // namespace ax

#endif