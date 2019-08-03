//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "parser.hh"

#include "exceptions.hh"

#include <boost/log/trivial.hpp>

namespace ax {

Parser::Parser(Lexer& lex)
    : lexer(lex)
{
}

Expr mkSymbolInt(string atom)
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
    auto tok = lexer.get_token();
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
        case TokenType::eof:
            return { top, true };
        default:
            x = sF;
        }
        if (x.type() != typeid(nullptr_t)) {
            top.push_back(x);
        }

        tok = lexer.get_token();
        BOOST_LOG_TRIVIAL(trace) << "parse_list: " << tok << " list: " << top.size();
    }
}

ParserResult Parser::parse()
{
    auto tok = lexer.get_token();
    BOOST_LOG_TRIVIAL(trace) << "parse: " << tok;
    switch (tok.type) {
    case TokenType::open:
        return parse_list();
    case TokenType::close:
        throw ParseException("Unexpected )");
        break;
    case TokenType::atom: {
        auto x = mkSymbolInt(tok.val);
        return { x, false };
    }
    case TokenType::eof:
        return { sF, true };
    default:
        return { sF, false };
    }
}
}