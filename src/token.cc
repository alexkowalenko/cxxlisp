//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "token.hh"
#include "expr.hh"

#include <iostream>

namespace ax {

Token::operator std::string() const {
    switch (type) {
    case TokenType::open:
        return "(";
    case TokenType::close:
        return ")";
    case TokenType::dot:
        return ".";
    case TokenType::quote:
        return "\"";
    case TokenType::backquote:
        return "`";
    case TokenType::comma:
        return ",";
    case TokenType::at:
        return "@";
    case TokenType::hash:
        return "#" + get<std::string>(val);
    case TokenType::atom:
        return get<std::string>(val);
    case TokenType::string:
        return "\"" + get<std::string>(val) + "\"";
    case TokenType::eof:
        return "eof";
    default:
        return std::string();
    }
}

std::ostream &operator<<(std::ostream &os, const Token &t) {
    return os << std::string(t);
}
} // namespace ax