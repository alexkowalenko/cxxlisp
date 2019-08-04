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

namespace {
    const string lispIdentifiers = "-+*/<=>!?:$%_&~^@.\\{}";

    bool isID(char c)
    {
        return isalnum(c) || lispIdentifiers.find(c) != string::npos;
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
        wchar_t c;
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
            wchar_t r;
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
        return Token();
    };
};

wchar_t Lexer::peek()
{
    return lineReader.peek_char();
};

wchar_t Lexer::scan()
{
    return lineReader.get_char();
    ;
};
}