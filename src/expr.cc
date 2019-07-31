//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <expr.hh>
namespace ax {

using boost::any_cast;

bool is_Atom(const Expr& s)
{
    return s.type() == typeid(Atom);
};

bool is_Int(const Expr& s)
{
    return s.type() == typeid(Int);
};

bool is_List(const Expr& s)
{
    return s.type() == typeid(List);
};

List as_List(const Expr& s)
{
    return any_cast<List>(s);
}
}
