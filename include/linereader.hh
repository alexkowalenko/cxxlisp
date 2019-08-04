//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef LINEREADER_HH
#define LINEREADER_HH

namespace ax {

class LineReader {
public:
    virtual ~LineReader(){};

    virtual wchar_t get_char() = 0;
    virtual wchar_t peek_char() = 0;
    virtual void push_char(wchar_t c) = 0;
};

inline wchar_t operator>>(LineReader& r, wchar_t& c)
{
    return c = r.get_char();
}
}
#endif