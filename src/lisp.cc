//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "lisp.hh"

#include <iostream>

#include "evaluator.hh"
#include "exceptions.hh"
#include "linereaderRL.hh"
#include "linereaderReplxx.hh"
#include "linereaderStream.hh"
#include "parser.hh"
#include "primitive.hh"
#include "symboltable.hh"

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
    init_prims();
};

void Lisp::repl(istream& istr, ostream& ostr)
{
    unique_ptr<LineReader> rl;
    if (opt.readline) {
        // rl = make_unique<LineReaderReadLine>();
        rl = make_unique<LineReaderReplxx>();
    } else {
        rl = make_unique<LineReaderStream>(istr);
    }
    Lexer lex(*rl);
    Parser parser(lex);
    Evaluator evaluator;

    while (true) {
        ParserResult res;
        try {
            res = parser.parse();
            if (res.eof) {
                break;
            }
            if (opt.parse_only) {
                cout << res.val << endl;
                continue;
            }

            auto ex = evaluator.eval(res.val, symboltable);
            ostr << ex << endl;

        } catch (UnknownToken& e) {
            ostr << "Unknown token: " << e.tok << endl;
            continue;
        } catch (ParseException& e) {
            ostr << "Parse error: " << e.what() << endl;
            continue;
        } catch (EndBracketException& e) {
            ostr << "Parse error: Extra bracket found" << endl;
            continue;
        } catch (EvalException& e) {
            ostr << "Eval error: " << e.what() << endl;
        } catch (NumericException& e) {
            ostr << "Numeric exception: " << e.what() << endl;
        } catch (RuntimeException& e) {
            ostr << "Runtime exception: " << e.what() << endl;
        } catch (exception& e) {
            ostr << "Exception: " << e.what() << endl;
            continue;
        } catch (...) {
            ostr << "Unknown exception!" << endl;
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