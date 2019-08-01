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
};

Options* getOptions(int argc, char* argv[]);

} // namespace ax

#endif