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
    LineReaderStream(istream& is);

    uint32_t get_char() override;
    uint32_t peek_char() override;
    void push_char(uint32_t c) override;

private:
    void get_line();

    istream& is;
};
}
#endif