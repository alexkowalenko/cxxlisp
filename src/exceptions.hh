//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <exception>
#include <string>

namespace ax {

class EOFException : public std::exception {};

class UnknownToken : public std::exception {
  public:
    UnknownToken(char t) : tok(t){};
    char tok;
};

class NotInt : public std::exception {};

class AXException : public std::exception {
  public:
    AXException(const std::string &s) : whatStr(s){};

    const char *what() const noexcept override { return whatStr.c_str(); }

  protected:
    std::string whatStr;
};

class ParseException : public AXException {
  public:
    ParseException(const std::string &e) : AXException(e){};
};

class EndBracketException : std::exception {};

class EvalException : public AXException {
  public:
    EvalException(const std::string &s) : AXException(s) {}
};

class NumericException : public AXException {
  public:
    NumericException(const std::string &s) : AXException(s) {}
};

class RuntimeException : public AXException {
  public:
    RuntimeException(const std::string &s) : AXException(s) {}
};

class ExceptionQuit : public std::exception {
  public:
    ExceptionQuit(long v) : val(v){};
    long val = 0;
};

} // namespace ax
