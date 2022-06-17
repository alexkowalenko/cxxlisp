//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <clocale>
#include <iostream>

#include "lisp.hh"
#include "options.hh"

#define SPDLOG_ACTIVE_LEVEL SPDLOG_LEVEL_DEBUG
#include "spdlog/spdlog.h"

using namespace ax;

void init_logging() {
    spdlog::set_level(spdlog::level::debug); // Set global log level to debug 
    // SPDLOG_DEBUG("Debug logging");  
}

int main(int argc, char *argv[]) {
    setlocale(LC_ALL, "en_US.utf8");
    init_logging();

    // Get options
    ax::Options options = getOptions(argc, argv);

    Lisp lispInterp = Lisp(options);
    lispInterp.init();
    lispInterp.repl(std::cin, std::cout);
    lispInterp.terminate();

    return 0;
}