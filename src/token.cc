//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "token.hh"

#include <iostream>

namespace ax {

using namespace std;

Token::operator string() const
{
    switch (type) {
    case TokenType::open:
        return "("s;
    case TokenType::close:
        return ")"s;
    case TokenType::dot:
        return "."s;
    case TokenType::quote:
        return "\""s;
    case TokenType::backquote:
        return "`"s;
    case TokenType::comma:
        return ","s;
    case TokenType::at:
        return "@"s;
    case TokenType::hash:
        return "#"s + val;
    case TokenType::atom:
        return val;
    case TokenType::string:
        return "\"" + val + "\"";
    case TokenType::eof:
        return "eof"s;
    default:
        return ""s;
    }
}

ostream& operator<<(ostream& os, const Token& t)
{
    return os << string(t);
}
}