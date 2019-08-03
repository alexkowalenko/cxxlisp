//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "parser.hh"

#include "exceptions.hh"

namespace ax {

Parser::Parser(Lexer& lex)
    : lexer(lex)
{
}

Expr Parser::parse()
{
    auto tok = lexer.get_token();
    if (tok.type == TokenType::eof) {
        throw EOFException();
    }
    return List({});
}
}