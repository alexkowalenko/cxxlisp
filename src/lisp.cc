//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "lisp.hh"

#include <iostream>

#include "exceptions.hh"
#include "linereaderRL.hh"
#include "linereaderReplxx.hh"
#include "linereaderStream.hh"
#include "parser.hh"

namespace ax {

Lisp::Lisp(const Options& o)
    : opt(o)
{
}

void Lisp::init()
{
    if (!opt.silent) {
        cout << "Hello C++ Lisp 👾 !" << endl;
    }
};

void Lisp::repl(ostream& ostr)
{
    unique_ptr<LineReader> rl;
    if (opt.readline) {
        // rl = make_unique<LineReaderReadLine>();
        rl = make_unique<LineReaderReplxx>();
    } else {
        rl = make_unique<LineReaderStream>(cin);
    }
    Lexer lex(*rl);
    Parser parser(lex);

    while (true) {
        ParserResult res;
        try {
            res = parser.parse();
            ostr << "> ";
            if (opt.parse_only) {
                cout << res.val << endl;
                if (res.eof) {
                    break;
                }
                continue;
            }
            ostr << res.val << endl;

        } catch (UnknownToken& e) {
            cerr << "Unknown token: " << e.tok << endl;
            continue;
        } catch (ParseException& e) {
            cerr << "Parse error: " << e.what() << endl;
            continue;
        } catch (EndBracketException& e) {
            cerr << "Parse error: Extra bracket found" << endl;
            continue;
        } catch (exception& e) {
            cerr << "Exception: " << e.what() << endl;
            continue;
        } catch (...) {
            cerr << "Unknown exception!" << endl;
            continue;
        }
        if (res.eof) {
            break;
        }
    }
}

void Lisp::terminate()
{
    if (!opt.silent) {
        cout << endl
             << "Bye 👾" << endl;
    }
};
}