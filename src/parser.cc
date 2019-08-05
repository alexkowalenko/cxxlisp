//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "parser.hh"

#include "exceptions.hh"

#include <boost/log/trivial.hpp>

namespace ax {

const Atom quote_atom = Atom("quote");
const Atom backquote_atom = Atom("backquote");
const Atom unquote_atom = Atom("unquote");
const Atom splice_unquote_atom = Atom("splice-unquote");

Parser::Parser(Lexer& lex)
    : lexer(lex)
{
}

Expr mkSymbolInt(string& atom)
{
    if (atom == "nil") {
        return sF;
    } else if (atom == "t") {
        return sT;
    }
    return Atom(atom);
}

Expr Parser::parse_comma()
{
    if (lexer.peek() == '@') {
        lexer.get_token();
        return splice_unquote_atom;
    }
    return unquote_atom;
}

ParserResult Parser::parse_quote(Token& tok)
{
    auto x = List{};
    if (tok.type == TokenType::quote) {
        x.push_back(quote_atom);
    } else {
        x.push_back(backquote_atom);
    }
    auto [next, eof] = parse();
    x.push_back(next);
    return { x, eof };
}

ParserResult Parser::parse_list()
{
    bool eof = false;
    List top = List{};
    while (true) {
        try {
            auto res = parse();
            eof = res.eof;
            if (!is_a<nullptr_t>(res.val)) {
                top.push_back(res.val);
            }
        } catch (EndBracketException) {
            return { top, eof };
        }
    }
}

ParserResult Parser::parse()
{
    Token tok;
    lexer >> tok;
    BOOST_LOG_TRIVIAL(trace) << "parse: " << tok;
    switch (tok.type) {
    case TokenType::open:
        return parse_list();

    case TokenType::close:
        throw EndBracketException();

    case TokenType::atom:
        return { mkSymbolInt(tok.val), false };

    case TokenType::quote:
    case TokenType::backquote:
        return parse_quote(tok);

    case TokenType::comma: {
        return { parse_comma(), false };
    }
    case TokenType::eof:
        return { sF, true };

    default:
        throw ParseException("Unknown token "s + string(tok));
    }
}
}