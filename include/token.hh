//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <variant>

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
    Token(TokenType t, wstring ws)
        : type(t)
        , val(ws){};

    explicit operator string() const;
    friend ostream& operator<<(ostream& os, const Token& t);

    TokenType type;
    variant<string, wstring> val;
};

ostream& operator<<(ostream& os, const Token& t);
}

#endif