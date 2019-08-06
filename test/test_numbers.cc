//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_numbers

#include "test.hh"

#include <limits>

#include <boost/format.hpp>
#include <boost/test/unit_test.hpp>

#include "expr.hh"

using namespace std;

struct TestEval;
void test_Evaluator(const vector<TestEval>& tests);

BOOST_AUTO_TEST_CASE(test_eval)
{
    vector<TestEval> tests = {
        { "1", "1" },
        { "+246443", "246443" },
        { "-912026331", "-912026331" },
        { "0", "0" },
        { "-0", "0" },
        { "+0", "0" },

        { to_string(numeric_limits<long>::min()), "-9223372036854775808" },
        { to_string(numeric_limits<long>::min() + 1), "-9223372036854775807" },
        { to_string(numeric_limits<long>::max()), "9223372036854775807" },
        { to_string(numeric_limits<long>::max() - 1), "9223372036854775806" },

        { "(atom 3)", "t" },
        { "(car '(1 2 3))", "1" },
        { "(cdr '(2 3 4))", "(3 4)" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_numberp)
{
    auto fmt = boost::format("(numberp %1%)");
    auto fmp = boost::format("(integerp %1%)");
    vector<TestEval> tests = {
        // numberp
        { "(numberp 0)", "t" },
        { "(numberp 1)", "t" },
        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "t" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "t" },
        //{ "(numberp (+ 2 3))", "t" },
        { "(numberp 'a)", "nil" },
        { "(numberp '(a b c))", "nil" },
        { "(numberp t)", "nil" },
        { "(numberp nil)", "nil" },

        { "(numberp)", "Eval error: numberp expecting an argument" },
        { "(numberp nil nil)", "Eval error: numberp expecting an argument" },

        // integerp
        { "(integerp 0)", "t" },
        { "(integerp 1)", "t" },
        { boost::str(fmp % (numeric_limits<long>::min())), "t" },
        { boost::str(fmp % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmp % (numeric_limits<long>::max())), "t" },
        { boost::str(fmp % (numeric_limits<long>::max() - 1)), "t" },
        //{ "(integerp (+ 2 3))", "t" },
        //{ "(integerp 3.145926536)", "nil" },
        //{ "(integerp #C(1 2))", "nil" },
        { "(integerp 'a)", "nil" },
        { "(integerp '(a b c))", "nil" },
        { "(integerp t)", "nil" },
        { "(integerp nil)", "nil" },

        { "(integerp)", "Eval error: integerp expecting an argument" },
        { "(integerp nil nil)", "Eval error: integerp expecting an argument" },
    };
    test_Evaluator(tests);
}