//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef OPTIONS_HH
#define OPTIONS_HH

#include <stack>

namespace ax {

using namespace std;

class Options {
public:
    bool silent = false;
    bool readline = true;
    bool parse_only = false;

    // Debug options
    bool debug_expr = false;

    void push_options();
    void pop_options();

private:
    stack<bool> saved_options;
};

Options
getOptions(int argc, char* argv[]);

} // namespace ax

#endif