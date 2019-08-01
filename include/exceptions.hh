//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef EXCEPTION_HH
#define EXCEPTION_HH

#include <exception>

namespace ax {

using namespace std;

class EOFException : exception {
};

class UnknownToken : exception {
public:
    UnknownToken(char t) { tok = t; };
    char tok;
};
}
#endif
