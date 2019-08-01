//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <iostream>

#include "lisp.hh"
#include "options.hh"

using namespace std;
using namespace ax;

int main(int argc, char* argv[])
{
    // Get options
    ax::Options* options = getOptions(argc, argv);

    if (!options->silent) {
        cout << "Hello C++ Lisp 👾 !" << endl;
    }

    Lisp lispInterp = Lisp(options);
    lispInterp.init();
    lispInterp.repl(cout);
    lispInterp.terminate();

    delete options;
    return 0;
}