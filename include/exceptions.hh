//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef EXCEPTION_HH
#define EXCEPTION_HH

#include <exception>
#include <string>

namespace ax {

using namespace std;

class EOFException : exception {
};

class UnknownToken : exception {
public:
    UnknownToken(char t)
        : tok(t){};
    char tok;
};

class ParseException : exception {
public:
    ParseException(string e)
        : error(e){};

    const char* what() const noexcept override
    {
        return error.c_str();
    }
    string error;
};

class EndBracketException : exception {
};
}
#endif
