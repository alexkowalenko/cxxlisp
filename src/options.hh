//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <stack>

namespace ax {

class Options {
  public:
    bool silent{false};
    bool readline{true};
    bool parse_only{false};

    // Debug options
    bool debug_expr{false};

    void push_options();
    void pop_options();

  private:
    std::stack<bool> saved_options;
};

int getOptions(int argc, char *argv[], Options &options);

} // namespace ax
