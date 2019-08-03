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
    ~LineReaderReadLine();

    wchar_t get_char() override;
    wchar_t peek_char() override;
    void push_char(wchar_t c) override;

private:
    void get_line();

    string buf;
    int ptr;

    string my_history_file;
};
}
#endif