//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef OPTIONS_HH
#define OPTIONS_HH

namespace ax {

class Options {
public:
    bool silent = false;
    bool readline = true;
    bool parse_only = false;

    // Debug options
    bool debug_expr = false;
};

Options
getOptions(int argc, char* argv[]);

} // namespace ax

#endif