//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef LINEREADER_HH
#define LINEREADER_HH

#include <cstdint>
#include <string>

namespace ax {

using namespace std;

class LineReader {
public:
    virtual ~LineReader(){};

    virtual uint32_t get_char() = 0;
    virtual uint32_t peek_char() = 0;
    virtual void push_char(uint32_t c) = 0;

protected:
    string buf;
    string::iterator ptr;
};

inline uint32_t operator>>(LineReader& r, uint32_t& c)
{
    return c = r.get_char();
}
}
#endif