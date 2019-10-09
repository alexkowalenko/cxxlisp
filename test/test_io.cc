//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_io

#include "test.hh"

#include <boost/test/unit_test.hpp>

using namespace std;

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
        { R"((prin1 "string" x))", "\"string\"" },
        { "(terpri x)", "t" },
        { R"((princ "string2" x))", "\"string2\"" },
        { R"((print "string3" x))", "\"string3\"" },
        { "(write-char #\\x x)", "#\\x" },
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
        { R"( (princ "Here there! 🍌🍊🍎" x) )", "\"Here there! 🍌🍊🍎\"" },
        { "(terpri x)", "t" },
        { "(close x)",
            "t" },
        { R"( (defvar y (open "/var/tmp/z.tmp" :direction :input)) )",
            "y" },
        { "(read-char y)", "#\\H" },
        { "(read-line y)", "\"ere there! 🍌🍊🍎\"" },
        { "(close y)", "t" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_format_1)
{
    vector<TestEval> tests = {
        { R"((format nil ""))", R"("")" },
        { R"((format nil "x"))", "\"x\"" },

        { R"((format nil "Hello,~%"))", "\"Hello,\n\"" },
        { R"((format nil "~&Hello,~%"))", "\"\nHello,\n\"" },

        { R"((format nil "Hello,~"))", "Eval error: format: incomplete ~" },

        { R"((format nil))", "Eval error: format expecting at least 2 arguments" },
        { R"((format nil 1))", "Eval error: format: format is not a string 1" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_format_2)
{
    vector<TestEval> tests = {
        { R"((format nil "Hello, ~S!~%" 'jones))", "\"Hello, jones!\n\"" },
        { R"((format nil "Hello, ~S!~%" 'jones 1))", "\"Hello, jones!\n\"" },
        { R"((format nil "Hello, ~S!~%" '(bob jim anne) 1))", "\"Hello, (bob jim anne)!\n\"" },

        { R"((format nil "Hello, ~S!~%" "jones"))", "\"Hello, \"jones\"!\n\"" },
        { R"((format nil "Hello, ~S!~%" #\j))", "\"Hello, #\\j!\n\"" },

        { R"((format nil "Hello, ~A!~%" 'jones))", "\"Hello, jones!\n\"" },
        { R"((format nil "Hello, ~A!~%" 'jones 1))", "\"Hello, jones!\n\"" },
        { R"((format nil "Hello, ~A!~%" '(bob jim anne) 1))", "\"Hello, (bob jim anne)!\n\"" },

        { R"((format nil "Hello, ~A!~%" "jones"))", "\"Hello, jones!\n\"" },
        { R"((format nil "Hello, ~A!~%" #\j))", "\"Hello, j!\n\"" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_format_3)
{
    vector<TestEval> tests = {
        { R"((format nil "Hello, ~S, ~S, ~S!~%" 1 2 3))", "\"Hello, 1, 2, 3!\n\"" },

        { R"((format nil "Hello, ~S, ~S, ~S!~%" 1 2 3 4))", "\"Hello, 1, 2, 3!\n\"" },
        { R"((format nil "Hello, ~S, ~S, ~S!~%" 1 2))", "Eval error: format: no argument ~S 3" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_format_4)
{
    vector<TestEval> tests = {
        { R"((format nil "Hello, ~S, ~S, ~S!~%" 1 2 3))", "\"Hello, 1, 2, 3!\n\"" },
        { R"((format t "Hello, ~S, ~S, ~S!~%" 1 2 3))", "t" },
        { R"((defvar x (open "/tmp/format.tmp" :direction :output)))", "x" },
        { R"((format x "Hello, ~S, ~S, ~S!~%" 1 2 3))", "t" },
        { R"((close x))", "t" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_trace)
{
    vector<TestEval> tests = {
        { "(trace)", "nil" },
        { "(defun f (x) x)", "f" },
        { "(defun g(x ) (f x))", "g" },
        { "(trace f g)", "(f g)" },
        { "(trace h)", "Eval error: trace: not a function h" },
        { "(trace)", "(f g)" },

        { "(g 2)", "2" },

        { "(untrace g)", "t" },
        { "(untrace f)", "t" },
        { "(untrace h)", "t" },

        { "(untrace)", "t" },
        { "(trace)", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_load)
{
    vector<TestEval> tests = {
        // source
        { "(load \"../demo/a.lisp\")", "t" },

        { "(g 3)", "5" }, // we really loaded it
        { "(load \"x.scm\")", "Eval error: load: can't open x.scm" },
        { "(load)", "Eval error: load expecting an argument" },
    };
    test_Evaluator(tests);
}
