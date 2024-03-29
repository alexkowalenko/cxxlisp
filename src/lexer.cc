//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "lexer.hh"

#include <cctype>
#include <iostream>
#include <string>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshadow"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wold-style-cast"
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wcast-align"
#include <unicode/uchar.h>
#include <utf8.h>
#pragma clang diagnostic pop

#include "exceptions.hh"

namespace ax {

namespace {

constexpr bool is_emoji(uint32_t c) {
    return (0x1f600 <= c && c <= 0x1f64f) || // Emoticons
           (0x1F300 <= c && c <= 0x1F5FF) || // Misc Symbols and Pictographs
           (0x1F680 <= c && c <= 0x1F6FF) || // Transport and Map
           (0x1F1E6 <= c && c <= 0x1F1FF) || // Regional country flags
           (0x2600 <= c && c <= 0x26FF) ||   // Misc symbols
           (0x2700 <= c && c <= 0x27BF) ||   // Dingbats
           (0xE0020 <= c && c <= 0xE007F) || // Tags
           (0xFE00 <= c && c <= 0xFE0F) ||   // Variation Selectors
           (0x1F900 <= c && c <= 0x1F9FF) || // Supplemental Symbols and Pictographs
           (0x1F018 <= c && c <= 0x1F270) || // Various asian characters
           (0x238C <= c && c <= 0x2454) ||   // Misc items
           (0x20D0 <= c && c <= 0x20FF);
}

const std::string lispIdentifiers = "-+*/<=>!?:$%_&~^@.\\{}";
const std::string hashChars = "\\('+-";

inline bool isID(uint32_t c) {
    return u_isalnum(UChar32(c)) || lispIdentifiers.find(char(c)) != std::string::npos ||
           is_emoji(c);
}
} // namespace

// avoid recreating standard tokens
static const Token open_token(TokenType::open);
static const Token close_token(TokenType::close);
static const Token quote_token(TokenType::quote);
static const Token backquote_token(TokenType::backquote);
static const Token comma_token(TokenType::comma);
static const Token at_token(TokenType::at);
static const Token dot_token(TokenType::dot);
static const Token null_token = Token();

Token Lexer::get_token() {
    try {
    top:
        uint32_t c;
        lineReader >> c;
        switch (c) {
        case '(':
            return open_token;
        case ')':
            return close_token;
        case '\'':
            return quote_token;
        case '`':
            return backquote_token;
        case ',':
            return comma_token;
        case '@':
            return at_token;

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
                for (auto t = lineReader.get_char(); !(t == '#' && prev == '|');
                     t = lineReader.get_char()) {
                    prev = t;
                }
                goto top;
            } else if (u_isalnum(UChar32(c)) || hashChars.find(char(c)) != std::string::npos) {
                Token result = Token(TokenType::hash);
                result.val = std::string(1, char(c));
                return result;
            }
            throw UnknownToken(char(c));
        case '"': {
            // build string
            std::wstring str;
            uint32_t     r;
            auto         prev = c;
            for (lineReader >> r; !(r == '\"' && prev != '\\') && r != '\n'; lineReader >> r) {
                if (prev == '\\' && r == '\"') {
                    str.pop_back();
                }
                str.push_back(wchar_t(r));
                prev = r;
            }
            return Token(TokenType::string, str);
        }
        case '.':
            auto n = lineReader.peek_char();
            if (isspace(char(n)) || int(n) == std::char_traits<char>::eof()) {
                return dot_token;
            }
            // fallthrough to get the atom
        };
        // BOOST_LOG_TRIVIAL(trace) << "lexer char " << c;
        if (isID(c)) {
            std::string id;
            utf8::append(char32_t(c), id);
            for (auto r = lineReader.peek_char(); isID(r); r = lineReader.peek_char()) {
                lineReader.get_char();
                utf8::append(char32_t(r), id);
            }
            // BOOST_LOG_TRIVIAL(trace) << "lexer token " << id;
            return Token(TokenType::atom, id);
        }
        if (isspace(char(c)) || c == 0) {
            goto top;
        }
        throw UnknownToken(char(c));
    } catch (EOFException &e) {
        return null_token;
    };
};

uint32_t Lexer::peek() {
    return lineReader.peek_char();
};

uint32_t Lexer::scan() {
    return lineReader.get_char();
    ;
};
} // namespace ax