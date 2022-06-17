//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "parser.hh"

#include <charconv>

#include <boost/algorithm/string.hpp>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshadow"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wold-style-cast"
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wcast-align"
#include <utf8.h>
#pragma clang diagnostic pop

#include "exceptions.hh"
#include "function.hh"
#include "primitive.hh"

namespace ax {

static const NotInt not_int;

const Atom lambda_atom = "lambda";

Int atoi(const std::string &str) {
    Int  value = 0;
    Int  sign = 1;
    bool digits = false;

    auto iter = str.begin();
    if (*iter == '+' || *iter == '-') {
        if (*iter == '-')
            sign = -1;
        iter++;
    }
    for (; isdigit(*iter); iter++) {
        value *= 10;
        value += int(*iter - '0');
        digits = true;
    }
    if (iter != str.end() || !digits) {
        throw not_int;
    }
    return value * sign;
}

Expr *mk_symbolInt(const std::string &atom) {
    if (atom == "nil") {
        return sF;
    } else if (atom == "t") {
        return sT;
    }
    if (atom[0] == '&' || atom[0] == ':') {
        return mk_keyword(atom);
    }
    if (atom.find('.') != std::string::npos) {
        try {
            return mk_float(stod(atom));
        } catch (std::invalid_argument) {
        } catch (std::out_of_range) {
            throw NumericException("float out of range " + atom);
        };
        // fallthrough to Int.
    }
    try {
        return mk_int(ax::atoi(atom));
    } catch (NotInt) { // fallthrough to be an atom
    };
    return mk_atom(atom);
}

Expr *Parser::parse_comma() {
    if (lexer.peek() == '@') {
        lexer.get_token();
        return splice_unquote_at;
    }
    return unquote_at;
}

Expr *Parser::parse_hash(const Token &tok) {
    auto token_val = boost::algorithm::to_upper_copy(std::get<std::string>(tok.val));
    if (token_val == "\\") {
        // character
        auto t = lexer.peek();
        if (!iswspace(wint_t(t))) {
            switch (t) {
            case 's':
            case 'S':
            case 'n':
            case 'N': {
                Token newTok = lexer.get_token();
                auto  val = boost::algorithm::to_lower_copy(std::get<std::string>(newTok.val));
                if (val == "newline") {
                    return mk_char('\n');
                } else if (val == "space") {
                    return mk_char(' ');
                }
                t = utf8::peek_next(std::string(newTok).begin(), std::string(newTok).end());
                return mk_char(wchar_t(t));
            }
            default:
                lexer.scan();
                return mk_char(wchar_t(t));
            }
        }
        throw ParseException("#\\ expecting a character");
    } else if (token_val == "'") {
        // function ref
        Token t = lexer.get_token();
        if (t.type == TokenType::atom) {
            return mk_function_ref(std::get<std::string>(t.val));
        } else if (t.type == TokenType::open) {
            auto funct = parse_list().val;
            if (!is_false(funct) && is_atom(funct->car) && funct->car->atom == lambda_atom &&
                !is_false(funct->cdr)) {
                auto f = lambda("lambda", funct->cdr);
                return f;
            }
            throw ParseException("#' expecting lambda expression " + to_string(funct));
        } else {
            throw ParseException("#' function ref unknown token " + std::string(t));
        }
    } else if (token_val == "B" || token_val == "O" || token_val == "X") {
        Token newTok = lexer.get_token();
        if (newTok.type != TokenType::atom) {
            throw ParseException("#" + token_val + " expecting number " + std::string(newTok));
        }
        int base = 10;
        if (token_val == "B") {
            base = 2;
        } else if (token_val == "O") {
            base = 8;
        } else if (token_val == "X") {
            base = 16;
        }
        Int  value = 0;
        auto str_val = std::get<std::string>(newTok.val);
        if (auto [p, ec] =
                std::from_chars(str_val.data(), str_val.data() + str_val.size(), value, base);
            ec == std::errc()) {
            return mk_int(value);
        } else {
            throw ParseException("#" + token_val + " badly formed number " + str_val);
        }
    } else if (token_val == "(") {
        // Vector
        auto list = parse_list();
        return to_vector(list.val);
    } else if (token_val == "C") {
        // Complex
        Token t = lexer.get_token();
        if (t.type != TokenType::open) {
            throw ParseException("malformed complex literal " + std::string(t));
        }
        auto list = parse_list();
        if (list.val->size() != 2) {
            throw ParseException("malformed complex literal" + to_string(list.val));
        }
        if (!is_number(list.val->car) || !is_number(list.val->cdr->car)) {
            throw ParseException("malformed complex literal" + to_string(list.val));
        }
        return mk_complex(Complex(as_float(list.val->car), (as_float(list.val->cdr->car))));
    }
    throw ParseException("# unknown " + std::string(tok));
}

ParserResult Parser::parse_quote(const Token &tok) {
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

ParserResult Parser::parse_list() {
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
            return {top, eof};
        }
    }
}

ParserResult Parser::parse() {
    Token tok;
    lexer >> tok;
    // BOOST_LOG_TRIVIAL(trace) << "parse: " << tok;

    switch (tok.type) {

    case TokenType::open:
        return parse_list();

    case TokenType::close:
        throw EndBracketException();

    case TokenType::atom:
        return {mk_symbolInt(std::get<std::string>(tok.val)), false};

    case TokenType::string:
        return {mk_string(std::get<std::wstring>(tok.val)), false};

    case TokenType::quote:
    case TokenType::backquote:
        return parse_quote(tok);

    case TokenType::dot: {
        auto res = parse();
        res.dot = true; // set dot to true.
        return res;
    }

    case TokenType::comma:
        return {parse_comma(), false};

    case TokenType::hash: {
        return {parse_hash(tok), false};
    }

    case TokenType::eof:
        return {sF, true};

    default:
        throw ParseException("Unknown token " + std::string(tok));
    }
}
} // namespace ax