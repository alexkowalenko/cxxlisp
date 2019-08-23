//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "parser.hh"

#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>
#include <utf8.h>

#include "exceptions.hh"
#include "function.hh"

namespace ax {

Parser::Parser(Lexer& lex)
    : lexer(lex)
{
}

Expr mkSymbolInt(const string& atom)
{
    if (atom == "nil") {
        return sF;
    } else if (atom == "t") {
        return sT;
    }
    if (atom[0] == '&' || atom[0] == ':') {
        return Keyword(atom);
    }
    if (atom.find('.') != string::npos) {
        try {
            return Float(stod(atom));
        } catch (invalid_argument) {
        } catch (out_of_range) {
        };
        // fallthrough to Int.
    }
    try {
        return Int{ stol(atom) };
    } catch (invalid_argument) { //fallthrough to be an atom
    } catch (out_of_range) {
    };
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

Expr Parser::parse_hash(const Token& tok)
{
    if (tok.val == "\\") {
        // character
        auto t = lexer.peek();
        if (!iswspace(t)) {
            switch (t) {
            case 's':
            case 'S':
            case 'n':
            case 'N': {
                Token newTok = lexer.get_token();
                auto val = boost::algorithm::to_lower_copy(newTok.val);
                if (val == "newline") {
                    return Char('\n');
                } else if (val == "space") {
                    return Char(' ');
                }
                t = utf8::peek_next(string(newTok).begin(), string(newTok).end());
                return Char(wchar_t(t));
            }
            default:
                lexer.scan();
                return Char(wchar_t(t));
            }
        }
        throw ParseException("#\\ expecting a character");
    } else if (tok.val == "'") {
        // function ref
        Token t = lexer.get_token();
        if (t.type == TokenType::atom) {
            return FunctionRef(t.val);
        } else {
            throw ParseException("#' function ref unknown token "s + string(t));
        }
    }
    throw ParseException("# unknown "s + string(tok));
}

ParserResult Parser::parse_quote(Token& tok)
{
    List x;
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
    List top;
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
    // BOOST_LOG_TRIVIAL(trace) << "parse: " << tok;
    switch (tok.type) {
    case TokenType::open:
        return parse_list();

    case TokenType::close:
        throw EndBracketException();

    case TokenType::atom:
        return { mkSymbolInt(tok.val), false };

    case TokenType::string:
        return { String(tok.val), false };

    case TokenType::quote:
    case TokenType::backquote:
        return parse_quote(tok);

    case TokenType::comma: {
        return { parse_comma(), false };
    }
    case TokenType::hash: {
        return { parse_hash(tok), false };
    }
    case TokenType::eof:
        return { sF, true };

    default:
        throw ParseException("Unknown token "s + string(tok));
    }
}
}