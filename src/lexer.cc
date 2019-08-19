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

    inline bool is_emoji(uint32_t c)
    {
        return (0x1f600 <= c && c <= 0x1f64f) || // Emoticons
            (0x1F300 <= c && c <= 0x1F5FF) || // Misc Symbols and Pictographs
            (0x1F680 <= c && c <= 0x1F6FF) || // Transport and Map
            (0x1F1E6 <= c && c <= 0x1F1FF) || // Regional country flags
            (0x2600 <= c && c <= 0x26FF) || // Misc symbols
            (0x2700 <= c && c <= 0x27BF) || // Dingbats
            (0xE0020 <= c && c <= 0xE007F) || // Tags
            (0xFE00 <= c && c <= 0xFE0F) || // Variation Selectors
            (0x1F900 <= c && c <= 0x1F9FF) || // Supplemental Symbols and Pictographs
            (0x1F018 <= c && c <= 0x1F270) || // Various asian characters
            (0x238C <= c && c <= 0x2454) || // Misc items
            (0x20D0 <= c && c <= 0x20FF);
    }

    const string lispIdentifiers = "-+*/<=>!?:$%_&~^@.\\{}";
    const string hashChars = "\\('+-";

    inline
    bool isID(uint32_t c)
    {
        // BOOST_LOG_TRIVIAL(trace) << "idID char " << char(c);
        return u_isalnum(c) || lispIdentifiers.find(c) != string::npos || is_emoji(c);
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

        case ';': {
            // comment
            uint32_t r;
            for (lineReader >> r; r != '\n'; lineReader >> r) {
            }
            goto top;
        }
        case '#':
            c = lineReader.get_char();
            if (c == '|') {
                // Multiline comment
                auto prev = c;
                for (auto t = lineReader.get_char(); !(t == '#' && prev == '|'); t = lineReader.get_char()) {
                    prev = t;
                }
                goto top;
            } else if (u_isalnum(c) || hashChars.find(c) != string::npos) {
                Token result = Token(TokenType::hash);
                result.val = ""s + char(c);
                return result;
            }
            throw UnknownToken(c);
        case '"': {
            // build string
            string str;
            uint32_t r;
            auto prev = c;
            for (lineReader >> r; !(r == '\"' && prev != '\\'); lineReader >> r) {
                if (prev == '\\' && r == '\"') {
                    str.pop_back();
                }
                utf8::append(char32_t(r), str);
                prev = r;
            }
            return Token(TokenType::string, str);
        }
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