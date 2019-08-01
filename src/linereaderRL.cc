//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "linereaderRL.hh"

#include "exceptions.hh"

#include <boost/format.hpp>
#include <boost/log/trivial.hpp>
#include <readline/readline.h>

namespace ax {

char const* prompt = "++> ";

LineReaderReadLine::LineReaderReadLine()
{
    ptr = -1;
}

wchar_t LineReaderReadLine::get_char()
{
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_char" << boost::format("buf: %1% ptr : %2%") % buf % ptr;
    if (ptr < 0 || ptr == int(buf.size())) {
        this->get_line();
    }
    // BOOST_LOG_TRIVIAL(trace) << "buf: " << buf;
    return buf[ptr++];
}

wchar_t LineReaderReadLine::peek_char()
{
    if (ptr < 0 || ptr == int(buf.size())) {
        get_line();
    }
    return buf[ptr];
}

void LineReaderReadLine::push_char(wchar_t)
{
    return;
}

void LineReaderReadLine::get_line()
{
    auto cbuf = readline(prompt);
    if (cbuf == nullptr) {
        throw EOFException();
    }
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_line: " << cbuf;
    buf = string(cbuf);
    buf.append(1, '\n');
    ptr = 0;
    free(cbuf);
}
}
