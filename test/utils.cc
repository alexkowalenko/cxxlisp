//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define SPDLOG_ACTIVE_LEVEL SPDLOG_LEVEL_DEBUG
#include <gtest/gtest.h>
#include <spdlog/spdlog.h>

#include <sstream>

#include "exceptions.hh"
#include "linereaderStream.hh"
#include "lisp.hh"

#include "test.hh"

using namespace ax;

void test_Evaluator(const std::vector<TestEval> &tests) {
    Options options;
    options.silent = true;
    options.readline = false;
    options.debug_expr = false;

    spdlog::set_level(spdlog::level::info); // Set global log level to info
    Lisp lisp(options);
    lisp.init();

    for (auto test : tests) {
        std::istringstream is(test.input);
        std::ostringstream out;

        // BOOST_TEST_CHECKPOINT(test.input);
        lisp.repl(is, out);

        auto result = out.str();
        result.pop_back(); // chop off \n
        std::cout << "eval: " << test.input << " -> " << result << std::endl;
        if (test.output != result) {
            std::cout << "\n"
                      << test.input << "\nshould be: " << result
                      << ", \n      not: " << test.output;
            FAIL();
            continue;
        }
    }
}
