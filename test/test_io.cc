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

BOOST_AUTO_TEST_CASE(test_eval_stream)
{
    vector<TestEval> tests = {
        { R"( (defvar x (open "/tmp/x.tmp" :direction :output)) )", "x" },
        { "(streamp x)", "t" },
        { "(output-stream-p x)", "t" },
        { "(input-stream-p x)", "nil" },
        { "(close x)", "t" },
        { "(close x)", "Eval error: close: can't close closed stream" },

        { R"( (defvar y (open "/tmp/x.tmp" :direction :input)) )", "y" },
        { "(streamp x)", "t" },
        { "(output-stream-p y)", "nil" },
        { "(input-stream-p y)", "t" },
        { "(close y)", "t" },
        { "(close y)", "Eval error: close: can't close closed stream" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_stream2)
{
    vector<TestEval> tests = {
        { R"((defvar x (open "/var/tmp/y.tmp" :direction :output)))", "x" },
        { "(output-stream-p x)", "t" },
        { "(input-stream-p x)", "nil" },
        { R"((prin1 "string" x))", "t" },
        { "(terpri x)", "t" },
        { R"((princ "string2" x))", "t" },
        { R"((print "string3" x))", "t" },
        //{ "(write-char #\\x x)", "t" },
        { "(terpri x)", "t" },
        { "(close x)", "t" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_stream3)
{
    vector<TestEval> tests = {
        { R"( (defvar x (open "/var/tmp/z.tmp" :direction :output)) )",
            "x" },
        { R"( (princ "string 🦑🦐🦀" x) )", "t" },
        { "(terpri x)", "t" },
        { "(close x)",
            "t" },
        { R"( (defvar y (open "/var/tmp/z.tmp" :direction :input)) )",
            "y" },
        // { "(read-char y)", "#\\s" },
        //{ "(read-char y)", "#\\t" },
        { "(close y)", "t" },
    };
    test_Evaluator(tests);
}
