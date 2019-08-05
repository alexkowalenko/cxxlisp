//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_parser
#include <boost/format.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/utility/setup.hpp>
#include <boost/test/unit_test.hpp>
#include <sstream>
#include <vector>

#include "exceptions.hh"
#include "linereaderStream.hh"
#include "lisp.hh"

using namespace ax;
using namespace std;
namespace logging = boost::log;

struct TestEval {
    string input;
    string output;
};

void test_Evaluator(const vector<TestEval>& tests);

BOOST_AUTO_TEST_CASE(test_eval)
{
    vector<TestEval> tests = {
        { "t", "t" },
        { "nil", "nil" },
    };

    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_quote)
{
    vector<TestEval> tests = {
        { "(quote a)", "a" },

        { "'a", "a" },
        { "'t", "t" },
        { "'nil", "nil" },
        { "()", "nil" },

        { "(quote)", "nil" },
        { "(quote (a b))", "(a b)" },

        { "a", "Eval error: Can't evaluate a" },
    };

    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_atom)
{
    vector<TestEval> tests = {
        { "(atom t)", "t" },
        { "(atom nil)", "t" },
        { "(atom (quote x))", "t" },
        { "(atom (quote (a b)))", "nil" },

        { "(atom t)", "t" },
        { "(atom ())", "t" },
        //{ "(atom (cdr '(a)))", "t" },
        //{ "(atom (cdr '(a b)))", "nil" },
    };

    test_Evaluator(tests);
}

void test_Evaluator(const vector<TestEval>& tests)
{
    Options options;
    options.silent = true;
    options.readline = false;

    logging::core::get()->set_filter(logging::trivial::severity >= logging::trivial::info);
    Lisp lisp(options);
    lisp.init();

    for (auto test : tests) {
        istringstream is(test.input);
        ostringstream out;

        BOOST_TEST_CHECKPOINT(test.input);
        lisp.repl(is, out);

        string result = out.str();
        result.erase(result.end() - 1, result.end()); // chop off \n
        cout << "eval: " << test.input << " : " << result << endl;
        if (test.output != result) {
            BOOST_ERROR(boost::format("\n%1% should be: %3%, \n      not:  %2%") % test.input % result % test.output);
            continue;
        }
    }
}
