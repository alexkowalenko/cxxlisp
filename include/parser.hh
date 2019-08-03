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

class Parser {
public:
    Parser(Lexer&);

    Expr parse();

private:
    Lexer& lexer;
};
}
#endif //
