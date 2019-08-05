//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>

namespace ax {

using namespace std;

enum class TokenType {
    open,
    close,
    dot,
    quote,
    backquote,
    comma,
    at,
    hash,
    atom,
    string,
    eof
};

class Token {
public:
    Token()
        : type(TokenType::eof){};
    Token(TokenType t)
        : type(t){};
    Token(TokenType t, string s)
        : type(t)
        , val(s){};

    explicit operator string() const;

    TokenType type;
    string val;
};

ostream& operator<<(ostream& os, const Token& t);
}

#endif