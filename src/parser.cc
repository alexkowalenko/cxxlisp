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

ParserResult Parser::parse_list()
{
    Token tok;
    lexer >> tok;
    // BOOST_LOG_TRIVIAL(trace) << "parse_list: " << tok;
    List top = List{};
    while (true) {
        Expr x = nullptr;
        switch (tok.type) {
        case TokenType::open: {
            auto res = parse_list();
            x = res.val;
            break;
        }
        case TokenType::close: {
            return { top, false };
        }
        case TokenType::atom: {
            x = mkSymbolInt(tok.val);
            break;
        }
        case TokenType::quote:
        case TokenType::backquote: {
            x = List{};
            if (tok.type == TokenType::quote) {
                as_a<List>(x).push_back(quote_atom);
            } else {
                as_a<List>(x).push_back(backquote_atom);
            }
            auto [next, eof] = parse();
            as_a<List>(x).push_back(next);
            break;
        }
        case TokenType::comma: {
            if (lexer.peek() == '@') {
                lexer.get_token();
                x = splice_unquote_atom;
            } else {
                x = unquote_atom;
            }
            break;
        }
        case TokenType::eof:
            return { top, true };
        default:
            x = sF;
        }
        if (!is_a<nullptr_t>(x)) {
            top.push_back(x);
        }

        // tok = lexer.get_token();
        lexer >> tok;
        BOOST_LOG_TRIVIAL(trace) << "parse_list: " << tok << " list: " << top.size();
    }
}

ParserResult Parser::parse()
{
    Expr cur = nullptr; // for quote objects, lists of atoms
    while (true) {
        Token tok;
        lexer >> tok;
        BOOST_LOG_TRIVIAL(trace) << "parse: " << tok;
        switch (tok.type) {
        case TokenType::open: {
            auto [x, res] = parse_list();
            if (is_a<nullptr_t>(cur)) {
                return { x, res };
            }
            as_a<List>(cur).push_back(x);
            return { cur, false };
        }
        case TokenType::close:
            throw ParseException("Unexpected )");
            break;
        case TokenType::atom: {
            auto x = mkSymbolInt(tok.val);
            if (is_a<nullptr_t>(cur)) {
                return { x, false };
            }
            as_a<List>(cur).push_back(x);

            return { cur, false };
        }
        case TokenType::quote:
            cur = List{ quote_atom };
            break;
        case TokenType::backquote: {
            cur = List{ backquote_atom };
            break;
        }
        case TokenType::comma: {
            if (lexer.peek() == '@') {
                lexer.get_token();
                return { splice_unquote_atom, false };
            }
            return { unquote_atom, false };
        }
        case TokenType::eof:
            return { sF, true };
        default:
            return { sF, false };
        }
    };
}
}