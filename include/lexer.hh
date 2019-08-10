//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef LEXER_HH
#define LEXER_HH

#include "linereader.hh"
#include "token.hh"

namespace ax {

class Lexer {
public:
    Lexer(LineReader&);
    Token get_token();

    uint32_t peek();
    uint32_t scan();

private:
    LineReader& lineReader;
};

inline Token operator>>(Lexer& l, Token& t)
{
    return t = l.get_token();
}
}
#endif