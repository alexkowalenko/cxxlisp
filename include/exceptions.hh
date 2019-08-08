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

class EOFException : public exception {
};

class UnknownToken : public exception {
public:
    UnknownToken(char t)
        : tok(t){};
    char tok;
};

class AXException : public exception {
public:
    AXException(const string& s)
        : whatStr(s){};

    const char* what() const noexcept override
    {
        return whatStr.c_str();
    }

protected:
    string whatStr;
};

class ParseException : public AXException {
public:
    ParseException(const string& e)
        : AXException(e){};
};

class EndBracketException : exception {
};

class EvalException : public AXException {
public:
    EvalException(const string& s)
        : AXException(s)
    {
    }
};

class NumericException : public AXException {
public:
    NumericException(const string& s)
        : AXException(s)
    {
    }
};

class RuntimeException : public AXException {
public:
    RuntimeException(const string& s)
        : AXException(s)
    {
    }
};
}
#endif