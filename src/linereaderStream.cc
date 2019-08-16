//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "linereaderStream.hh"

#include <array>

#include <boost/log/trivial.hpp>

#include "utf8.h"

#include "exceptions.hh"

namespace ax {

LineReaderStream::LineReaderStream(istream& s)
    : is(s)
{
    ptr = buf.end();
}

uint32_t LineReaderStream::get_char()
{
    //BOOST_LOG_TRIVIAL(trace) << "LineReader::get_char";
    while (ptr == buf.end()) {
        get_line();
    }
    //BOOST_LOG_TRIVIAL(trace) << "LineReader::get_char: >" << buf << "<";
    uint32_t c = utf8::next(ptr, buf.end());
    //BOOST_LOG_TRIVIAL(trace) << "get_char : " << c;
    return c;
}

uint32_t LineReaderStream::peek_char()
{
    while (ptr == buf.end()) {
        get_line();
    }
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::peek_char: >" << buf << "<";
    return utf8::peek_next(ptr, buf.end());
}

void LineReaderStream::push_char(uint32_t)
{
    utf8::advance(ptr, -1, buf.end());
    return;
}

void LineReaderStream::get_line()
{
    if (!getline(is, buf)) {
        throw EOFException();
    }
    buf.push_back('\n');
    ptr = buf.begin();
}
}
