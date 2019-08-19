//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_chars

#include "test.hh"

#include <limits>

#include <boost/format.hpp>
#include <boost/test/unit_test.hpp>

#include "expr.hh"

using namespace std;

struct TestEval;
void test_Evaluator(const vector<TestEval>& tests);

BOOST_AUTO_TEST_CASE(test_eval_chars)
{
    vector<TestEval> tests = {
        { "#\\a", "#\\a" },
        // { "#\\abs", "Repl Error: Not a character: abs" },
        { "#\\1", "#\\1" },
        { "#\\A", "#\\A" },
        { "#\\.", "#\\." },
        { "#\\;", "#\\;" },
        { "#\\(", "#\\(" },
        { "#\\)", "#\\)" },
        { "#\\;", "#\\;" },
        { "#\\#", "#\\#" },
        { "#\\\\", "#\\\\" },
        { "#\\ἄ", "#\\ἄ" },
        { "#\\七", "#\\七" },
        { "#\\👾", "#\\👾" },
        { "#\\space", "#\\space" },
        { "#\\newline", "#\\newline" },
        { "#\\SPACE", "#\\space" },
        { "#\\NeWlInE", "#\\newline" },

        { "'#\\a", "#\\a" },
        { "'#\\;", "#\\;" },
        { "'#\\space", "#\\space" },
        { "'#\\newline", "#\\newline" },
    };
    test_Evaluator(tests);
}