//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "lexer.hh"

#include <cctype>
#include <iostream>
#include <string>

#include "exceptions.hh"
#include "linereaderRL.hh"

namespace ax {

const string lispIdentifiers = "-+*/<=>!?:$%_&~^@.\\{}";

inline bool isID(char c)
{
    return isalnum(c) || lispIdentifiers.find(c) != string::npos;
}

Lexer::Lexer(LineReader& r)
    : lineReader(r)
{
}

Token Lexer::get_token()
{
    try {
    top:
        auto c = lineReader.get_char();
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
            for (auto r = lineReader.get_char(); r != '\n'; r = lineReader.get_char()) {
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
        if (isID(c)) {
            auto id = string(1, c);
            for (auto r = lineReader.peek_char(); isID(r); r = lineReader.peek_char()) {
                lineReader.get_char();
                id += r;
            }
            return Token(TokenType::atom, id);
        }
        if (isspace(c) || c == 0) {
            goto top;
        }
        cerr << "Unknown tokent " << c << endl;
        throw UnknownToken(c);
    } catch (EOFException& e) {
        return Token(TokenType::eof);
    };
};

wchar_t Lexer::peek()
{
    return wchar_t(0);
};

wchar_t Lexer::scan()
{
    return wchar_t(0);
};
}