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
    Expr parse_hash(const Token&);
    ParserResult parse_quote(Token&);

    Lexer& lexer;
};

const Atom quote_atom = Atom("quote");
const Atom backquote_atom = Atom("backquote");
const Atom unquote_atom = Atom("unquote");
const Atom splice_unquote_atom = Atom("splice-unquote");
}
#endif
