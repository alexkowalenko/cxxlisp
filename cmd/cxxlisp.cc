//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <iostream>

#include "options.hh"

using namespace std;
using namespace ax;

int main(int argc, char* argv[])
{
    // Get options
    Options* options = getOptions(argc, argv);

    if (!options->silent) {
        cout << "Hello C++ Lisp 👾 !" << endl;
    }
    return 0;
}