//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "linereaderStream.hh"

#include "exceptions.hh"

namespace ax {

wchar_t LineReaderStream::get_char()
{
    if (is.eof())
        throw EOFException();
    char ch;
    is >> ch;
    return ch;
}

wchar_t LineReaderStream::peek_char()
{
    if (is.eof())
        throw EOFException();
    return is.peek();
}

void LineReaderStream::push_char(wchar_t c)
{
    is.putback(c);
    return;
}
}
