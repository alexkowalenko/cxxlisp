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

typedef boost::any Expr;

typedef string Atom;
typedef bool Bool;
typedef long Int;
typedef list<boost::any> List;

bool is_Atom(const Expr& s);
bool is_Int(const Expr& s);
bool is_List(const Expr& s);

List as_List(const Expr& s);

// Output

ostream& operator<<(ostream& os, const Expr& e);

// Bool

const shared_ptr<Expr> sF = make_shared<Expr>(Bool(false));
const shared_ptr<Expr> sT = make_shared<Expr>(Bool(false));

} // namespace ax

#endif