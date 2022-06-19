//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "options.hh"

#include <iostream>

#define SPDLOG_ACTIVE_LEVEL SPDLOG_LEVEL_DEBUG
#include <spdlog/spdlog.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wimplicit-int-float-conversion"
#include <CLI/CLI.hpp>
#pragma clang diagnostic pop
namespace ax {

int getOptions(int argc, char *argv[], Options &options) {

    std::string debug;
    CLI::App    app{"cxxlisp: Interprète du Lisp"};

    app.add_flag("-s,--silent", options.silent, "silent, don't print the prompt");
    app.add_flag("-r,--noreadline", options.readline, "don't use readline for input");
    app.add_flag("-p,--parseonly", options.parse_only, "only parse the input and print result");
    app.add_option("-D,--debug", options.debug_expr, "debug options");

    CLI11_PARSE(app, argc, argv);
    return 0;
}

void Options::push_options() {
    saved_options.push(silent);
    saved_options.push(readline);
    saved_options.push(parse_only);
    saved_options.push(debug_expr);
};

void Options::pop_options() {
    debug_expr = saved_options.top();
    saved_options.pop();
    parse_only = saved_options.top();
    saved_options.pop();
    readline = saved_options.top();
    saved_options.pop();
    silent = saved_options.top();
    saved_options.pop();
}
} // namespace ax