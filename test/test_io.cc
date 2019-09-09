//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_io

#include "test.hh"

#include <limits>

#include <boost/format.hpp>
#include <boost/test/unit_test.hpp>

using namespace std;

struct TestEval;
void test_Evaluator(const vector<TestEval>& tests);

BOOST_AUTO_TEST_CASE(test_eval_error)
{
    vector<TestEval> tests = {
        { R"((error "This is an error") )", R"(Eval error: "This is an error")" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_io)
{
    vector<TestEval> tests = {
        { "*standard-output*", "<stream:output>" },
        { "(streamp *standard-output*)", "t" },
        { "(output-stream-p *standard-output*)", "t" },
        { "(input-stream-p *standard-output*)", "nil" },

        { "*standard-input*", "<stream:input>" },
        { "(streamp *standard-input*)", "t" },
        { "(output-stream-p *standard-input*)", "nil" },
        { "(input-stream-p *standard-input*)", "t" },

        { "*error-output*", "<stream:output>" },
        { "(streamp *error-output*)", "t" },
        { "(output-stream-p *error-output*)", "t" },
        { "(input-stream-p *error-output*)", "nil" },

        { "(streamp 1)", "nil" },
        { "(output-stream-p 'a)", "nil" },
        { "(input-stream-p nil)", "nil" },
    };
    test_Evaluator(tests);
}