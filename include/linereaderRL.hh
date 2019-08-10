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

    uint32_t get_char() override;
    uint32_t peek_char() override;
    void push_char(uint32_t c) override;

private:
    void get_line();

    string my_history_file;
};
}
#endif