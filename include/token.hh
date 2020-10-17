//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <string>
#include <variant>

namespace ax {

enum class TokenType { open, close, dot, quote, backquote, comma, at, hash, atom, string, eof };

class Token {
  public:
    Token() : type(TokenType::eof){};
    Token(TokenType t) : type(t){};
    Token(TokenType t, const std::string &s) : type(t), val(s){};
    Token(TokenType t, const std::wstring &ws) : type(t), val(ws){};

    explicit             operator std::string() const;
    friend std::ostream &operator<<(std::ostream &os, const Token &t);

    TokenType                               type;
    std::variant<std::string, std::wstring> val;
};

std::ostream &operator<<(std::ostream &os, const Token &t);

} // namespace ax