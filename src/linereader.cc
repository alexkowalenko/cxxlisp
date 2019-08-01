//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "linereader.hh"

#include "exceptions.hh"

#include <readline/readline.h>

namespace ax {

char const* prompt = "++> ";

LineReader::LineReader()
{
    ptr = -1;
}

void LineReader::InitScanner()
{
    return;
};

wchar_t LineReader::get_char()
{
    if (ptr < 0 || ptr == buf.size()) {
        get_line();
    }
    return buf[ptr++];
};

wchar_t LineReader::peek_char()
{
    if (ptr < 0 || ptr == buf.size()) {
        get_line();
    }
    return buf[ptr];
};

void LineReader::push_char(wchar_t)
{
    return;
};

void LineReader::get_line()
{
    auto cbuf = readline(prompt);
    if (cbuf == nullptr) {
        throw EOFException();
    }
    buf = string(cbuf);
    ptr = 0;
    free(cbuf);
}
}
