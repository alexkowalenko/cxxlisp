//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef LINEREADERSTREAM_HH
#define LINEREADERSTREAM_HH

#include "linereader.hh"

#include <iostream>

namespace ax {

using namespace std;

class LineReaderStream : public LineReader {
public:
    LineReaderStream(istream& is)
        : is(is){};

    wchar_t get_char() override;
    wchar_t peek_char() override;
    void push_char(wchar_t c) override;

private:
    istream& is;
};
}
#endif