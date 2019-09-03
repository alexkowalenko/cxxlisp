//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "function.hh"

namespace ax {

using namespace std;

Function::operator string()
{
    return "λ:"s + name;
}

/*
bool has_keyword(const List& args, const Keyword& k)
{
    auto it = find_if(args.begin(), args.end(), [&k](const Expr& e) -> bool {
        return is_a<Keyword>(e) && any_cast<Keyword>(e) == k;
    });
    return it != args.end();
}

Expr get_keyword_value(const List& args, const Keyword& k)
{
    auto it = find_if(args.begin(), args.end(), [&k](const Expr& e) -> bool {
        return is_a<Keyword>(e) && any_cast<Keyword>(e) == k;
    });
    if (it != args.end() || it != args.end() - 1) {
        return *(it + 1);
    }
    return sF;
}
*/
}