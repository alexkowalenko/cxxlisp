//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef LINEREADERRL_HH
#define LINEREADERRL_HH

#include <string>

#include "linereader.hh"

namespace ax {

using namespace std;

class LineReaderReadLine : public LineReader {
public:
    LineReaderReadLine();

    wchar_t get_char();
    wchar_t peek_char();
    void push_char(wchar_t c);

private:
    void get_line();

    string buf;
    int ptr;
};
}
#endif