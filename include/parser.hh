//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef PARSER_HH
#define PARSER_HH

#include "expr.hh"
#include "lexer.hh"

namespace ax {

struct ParserResult {
    Expr val;
    bool eof;
};

class Parser {
public:
    Parser(Lexer&);

    ParserResult parse();

private:
    ParserResult parse_list();

    Expr parse_comma();
    ParserResult parse_quote(Token&);

    Lexer& lexer;
};
}
#endif
