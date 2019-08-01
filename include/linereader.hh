//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef LINEREADER_HH
#define LINEREADER_HH

#include <string>

namespace ax {

using namespace std;

class LineReader {
public:
    LineReader();

    void InitScanner();

    wchar_t get_char();
    wchar_t peek_char();
    void push_char(wchar_t c);

private:
    void get_line();

    string buf;
    size_t ptr;
};
}
#endif