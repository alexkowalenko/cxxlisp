//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <iostream>

#include "lisp.hh"
#include "options.hh"

#include <boost/log/core.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/trivial.hpp>

using namespace std;
using namespace ax;
namespace logging = boost::log;

int main(int argc, char* argv[])
{
    // Get options
    ax::Options* options = getOptions(argc, argv);

    logging::core::get()->set_filter(logging::trivial::severity >= logging::trivial::trace);

    if (!options->silent) {
        cout << "Hello C++ Lisp 👾 !" << endl;
    }

    Lisp lispInterp = Lisp(options);
    lispInterp.init();
    lispInterp.repl(cout);
    lispInterp.terminate();

    delete options;
    if (!options->silent) {
        cout << endl
             << "Bye 👾" << endl;
    }
    return 0;
}