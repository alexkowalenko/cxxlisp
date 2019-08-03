//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef LINEREADERREPLXX_HH
#define LINEREADERREPLXX_HH

#include "linereader.hh"

#include <replxx.hxx>
#include <string>

namespace ax {

using namespace std;
using Replxx = replxx::Replxx;

class LineReaderReplxx : public LineReader {
public:
    LineReaderReplxx();
    ~LineReaderReplxx();

    wchar_t get_char() override;
    wchar_t peek_char() override;
    void push_char(wchar_t c) override;

private:
    void get_line();

    Replxx replxx;

    string buf;
    int ptr;

    string my_history_file;
};
}
#endif