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

static const NotInt not_int;

Int atoi(const string& str)
{
    Int value = 0;
    Int sign = 1;
    bool digits = false;

    auto iter = str.begin();
    if (*iter == '+' || *iter == '-') {
        if (*iter == '-')
            sign = -1;
        iter++;
    }
    for (; isdigit(*iter); iter++) {
        value *= 10;
        value += (int)(*iter - '0');
        digits = true;
    }
    if (iter != str.end() || !digits) {
        throw not_int;
    }
    return value * sign;
}

Expr* mk_symbolInt(const string& atom)
{
    if (atom == "nil") {
        return sF;
    } else if (atom == "t") {
        return sT;
    }
    if (atom[0] == '&' || atom[0] == ':') {
        return mk_keyword(atom);
    }
    if (atom.find('.') != string::npos) {
        try {
            return mk_float(stod(atom));
        } catch (invalid_argument) {
        } catch (out_of_range) {
            throw NumericException("float out of range " + atom);
        };
        // fallthrough to Int.
    }
    try {
        return mk_int(ax::atoi(atom));
    } catch (NotInt) { //fallthrough to be an atom
    };
    return mk_atom(atom);
}

Expr* Parser::parse_comma()
{
    if (lexer.peek() == '@') {
        lexer.get_token();
        return splice_unquote_at;
    }
    return unquote_at;
}

Expr* Parser::parse_hash(const Token& tok)
{
    if (get<string>(tok.val) == "\\") {
        // character
        auto t = lexer.peek();
        if (!iswspace(t)) {
            switch (t) {
            case 's':
            case 'S':
            case 'n':
            case 'N': {
                Token newTok = lexer.get_token();
                auto val = boost::algorithm::to_lower_copy(get<string>(newTok.val));
                if (val == "newline") {
                    return mk_char('\n');
                } else if (val == "space") {
                    return mk_char(' ');
                }
                t = utf8::peek_next(string(newTok).begin(), string(newTok).end());
                return mk_char(wchar_t(t));
            }
            default:
                lexer.scan();
                return mk_char(wchar_t(t));
            }
        }
        throw ParseException("#\\ expecting a character");
    } else if (get<string>(tok.val) == "'") {
        // function ref
        Token t = lexer.get_token();
        if (t.type == TokenType::atom) {
            return mk_function_ref(get<string>(t.val));
        } else {
            throw ParseException("#' function ref unknown token "s + string(t));
        }
    }
    throw ParseException("# unknown "s + string(tok));
}

ParserResult Parser::parse_quote(const Token& tok)
{
    auto x = mk_list();
    if (tok.type == TokenType::quote) {
        x->car = quote_at;
    } else {
        x->car = backquote_at;
    }
    auto res = parse();
    x->cdr = mk_list(res.val);
    res.val = x;
    return res;
}

ParserResult Parser::parse_list()
{
    auto top = mk_list();
    auto l = top;
    while (true) {
        bool eof = false;
        try {
            auto res = parse();
            eof = res.eof;
            if (res.val != nullptr) {
                if (!l->car) {
                    l->car = res.val;
                } else {
                    if (res.dot) {
                        l->cdr = res.val;
                    } else {
                        l->cdr = mk_list(res.val);
                    }
                    l = l->cdr;
                }
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
        return { mk_symbolInt(get<string>(tok.val)), false };

    case TokenType::string:
        return { mk_string(get<wstring>(tok.val)), false };

    case TokenType::quote:
    case TokenType::backquote:
        return parse_quote(tok);

    case TokenType::dot: {
        auto res = parse();
        res.dot = true; // set dot to true.
        return res;
    }

    case TokenType::comma:
        return { parse_comma(), false };

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