//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "lisp.hh"

#include <iostream>

#include "exceptions.hh"

namespace ax {

Lisp::Lisp(Options* opt)
{
    this->opt = opt;
    lex = Lexer();
}

void Lisp::init(){};

void Lisp::repl(ostream& ostr)
{
    lex.setup_lexer();
    try {
        while (true) {
            Token tok = lex.get_token();
            ostr << tok.val << endl;
        }
    } catch (UnknownToken& e) {
        cerr << "Unknown token: " << e.tok << endl
             << flush;
        return;

    } catch (exception& e) {
        cerr << "Exception: " << e.what() << endl
             << flush;
        return;
    } catch (...) {
        cerr << "Unknown excpetion!" << endl
             << flush;
        return;
    }
}

void Lisp::terminate(){};
}