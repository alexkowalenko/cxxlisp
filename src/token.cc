//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "token.hh"

#include <iostream>

namespace ax {

using namespace std;

Token::Token(TokenType t)
{
    type = t;
}

Token::Token(TokenType t, string s)
{
    type = t;
    val = s;
}

ostream& operator<<(ostream& os, const Token& t)
{
    switch (t.type) {
    case TokenType::open:
        os << '(';
        break;
    case TokenType::close:
        os << ')';
        break;
    case TokenType::dot:
        os << '.';
        break;
    case TokenType::quote:
        os << '\'';
        break;
    case TokenType::backquote:
        os << '`';
        break;
    case TokenType::comma:
        os << ',';
        break;
    case TokenType::at:
        os << '@';
        break;
    case TokenType::hash:
        os << '#';
        break;
    case TokenType::colon:
        os << ':';
        break;
    case TokenType::atom:
        os << t.val;
        break;
    case TokenType::string:
        os << '"' << t.val << '"';
        break;
    case TokenType::num:
        os << 'f' << t.val;
        break;
    case TokenType::eof:
        os << 'f' << t.val;
        break;
    default:
        break;
    }
    return os;
};
}