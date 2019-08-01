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
    colon,
    atom,
    string,
    num,
    eof
};

class Token {
public:
    Token(TokenType t);
    Token(TokenType t, string s);

    TokenType type;
    string val;
};

ostream& operator<<(ostream& os, const Token& t);
}
#endif