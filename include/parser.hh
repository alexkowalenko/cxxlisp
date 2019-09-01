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
    Expr* val;
    bool eof;
};

class Parser {
public:
    Parser(Lexer&);

    ParserResult parse();

private:
    ParserResult parse_list();

    Expr* parse_comma();
    // Expr parse_hash(const Token&);
    ParserResult parse_quote(Token&);

    Lexer& lexer;
};

inline Expr* quote_at = mk_atom("quote");
inline Expr* backquote_at = mk_atom("backquote");
inline Expr* unquote_at = mk_atom("unquote");
inline Expr* splice_unquote_at = mk_atom("splice-unquote");
}
#endif
