//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "lisp.hh"

#include <iostream>

#include "exceptions.hh"
#include "linereaderRL.hh"
#include "linereaderStream.hh"

namespace ax {

Lisp::Lisp(Options* opt)
{
    this->opt = opt;
}

void Lisp::init(){};

void Lisp::repl(ostream& ostr)
{
    LineReader* rl;
    if (opt->readline) {
        rl = new LineReaderReadLine();
    } else {
        rl = new LineReaderStream(cin);
    }
    Lexer lex(*rl);
    try {
        while (true) {
            Token tok = lex.get_token();
            if (tok.type == TokenType::eof) {
                break;
            }
            ostr << tok << endl;
        }
    } catch (UnknownToken& e) {
        cerr << "Unknown token: " << e.tok << endl
             << flush;
    } catch (exception& e) {
        cerr << "Exception: " << e.what() << endl
             << flush;
    } catch (...) {
        cerr << "Unknown exception!" << endl
             << flush;
    }
    delete rl;
}

void Lisp::terminate(){};
}