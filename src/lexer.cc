//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "lexer.hh"

#include <cctype>
#include <iostream>
#include <string>

#include <boost/log/trivial.hpp>
#include <unicode/uchar.h>

#include "utf8.h"

#include "exceptions.hh"
#include "linereaderRL.hh"

namespace ax {

namespace {
    const string lispIdentifiers = "-+*/<=>!?:$%_&~^@.\\{}";

    bool isID(uint32_t c)
    {
        // BOOST_LOG_TRIVIAL(trace) << "idID char " << char(c);
        return u_isalnum(c) || lispIdentifiers.find(c) != string::npos;
    }
}

Lexer::Lexer(LineReader& r)
    : lineReader(r)
{
}

Token Lexer::get_token()
{
    try {
    top:
        uint32_t c;
        lineReader >> c;
        switch (c) {
        case '(':
            return Token(TokenType::open);
        case ')':
            return Token(TokenType::close);
        case '\'':
            return Token(TokenType::quote);
        case '`':
            return Token(TokenType::backquote);
        case ',':
            return Token(TokenType::comma);
        case '@':
            return Token(TokenType::at);

        case ';':
            // comment
            uint32_t r;
            for (lineReader >> r; r != '\n'; lineReader >> r) {
            }
            goto top;
        case '#':
            return Token(TokenType::hash);
        case '.':
            auto n = lineReader.peek_char();
            if (isspace(n) || n == char_traits<char>::eof()) {
                return Token(TokenType::dot);
            }
            // fallthrough to get the atom
        };
        // BOOST_LOG_TRIVIAL(trace) << "lexer char " << c;
        if (isID(c)) {
            string id;
            utf8::append(char32_t(c), id);
            for (auto r = lineReader.peek_char(); isID(r); r = lineReader.peek_char()) {
                lineReader.get_char();
                utf8::append(char32_t(r), id);
            }
            // BOOST_LOG_TRIVIAL(trace) << "lexer token " << id;
            return Token(TokenType::atom, id);
        }
        if (isspace(c) || c == 0) {
            goto top;
        }
        cerr << "Unknown token " << c << endl;
        throw UnknownToken(c);
    } catch (EOFException& e) {
        return Token();
    };
};

uint32_t Lexer::peek()
{
    return lineReader.peek_char();
};

uint32_t Lexer::scan()
{
    return lineReader.get_char();
    ;
};
}