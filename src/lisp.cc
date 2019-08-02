//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "lisp.hh"

#include <iostream>

#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>

#include <readline/history.h>

#include "exceptions.hh"
#include "linereaderRL.hh"
#include "linereaderStream.hh"

namespace ax {

const string history_file = ".cxxlisp";
const int max_history = 1000;

Lisp::Lisp(Options* o)
    : opt(o)
{
}

void Lisp::init()
{
    if (opt->readline) {
        using_history();

        struct passwd* pw = getpwuid(getuid());
        my_history_file = string(pw->pw_dir);
        my_history_file += "/" + history_file;
        auto res = read_history(my_history_file.c_str());
        if (res != 0) {
            cerr << "Can't read the history file: " << my_history_file
                 << ' ' << strerror(res) << endl;
        }
    }
};

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

void Lisp::terminate()
{
    if (opt->readline) {

        auto res = write_history(my_history_file.c_str());
        if (res != 0) {
            cerr << "Can't write the history file: " << my_history_file
                 << ' ' << strerror(res) << endl;
        }
        res = history_truncate_file(my_history_file.c_str(), max_history);
        if (res != 0) {
            cerr << "Can't truncate the history file: " << history_file << ' ' << strerror(res) << endl;
        }
    }
};
}