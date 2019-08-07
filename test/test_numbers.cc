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

BOOST_AUTO_TEST_CASE(test_zerop)
{
    auto fmt = boost::format("(zerop %1%)");
    vector<TestEval> tests = {
        { "(zerop 0)", "t" },
        { "(zerop 0.0)", "t" },
        //{ "(zerop 0.0s0)", "t" },
        //{ "(zerop 0.0f0)", "t" },
        //{ "(zerop 0.0d0)", "t" },
        //{ "(zerop 0.0l0)", "t" },
        { "(zerop -0.0)", "t" },
        //{ "(zerop -0.0s0)", "t" },
        //{ "(zerop -0.0f0)", "t" },
        //{ "(zerop -0.0d0)", "t" },
        //{ "(zerop -0.0l0)", "t" },

        { "(zerop 1)", "nil" },
        //{ "(zerop (+ 2 3))", "nil" },
        { "(zerop -1)", "nil" },
        { boost::str(fmt % numeric_limits<long>::min()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "nil" },
        { boost::str(fmt % numeric_limits<long>::max()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "nil" },

        // Floats
        //{ "(zerop 3.145926536)", "nil" },
        //{ "(zerop -1.2345e-8)", "nil" },
        //{ fmt.Sprintf("(zerop %g)", math.MaxFloat32), "nil" },
        //{ fmt.Sprintf("(zerop %g)", math.MaxFloat64), "nil" },
        //{ fmt.Sprintf("(zerop %g)", math.SmallestNonzeroFloat32), "nil" },
        //{ fmt.Sprintf("(zerop %g)", math.SmallestNonzeroFloat64), "nil" },

        // Complex
        //{ "(zerop #C(0 0))", "t" },
        //{ "(zerop #C(0 1))", "nil" },

        { "(zerop 'a)", "Eval error: zerop argument needs to be number" },
        { "(zerop)", "Eval error: zerop expecting an argument" },
        { "(zerop 1 0)", "Eval error: zerop expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_plusp)
{
    auto fmt = boost::format("(plusp %1%)");
    vector<TestEval> tests = {
        { "(plusp 1)", "t" },
        //{ "(plusp (* 2 3))", "t" },
        { boost::str(fmt % numeric_limits<long>::min()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "nil" },
        { boost::str(fmt % numeric_limits<long>::max()), "t" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "t" },

        { "(plusp 0)", "nil" },
        { "(plusp -1)", "nil" },
        { "(plusp 0)", "nil" },
        { "(plusp 0.0)", "nil" },
        //{ "(plusp 0.0s0)", "nil" },
        //{ "(plusp 0.0f0)", "nil" },
        //{ "(plusp 0.0d0)", "nil" },
        //{ "(plusp 0.0l0)", "nil" },
        { "(plusp -0.0)", "nil" },
        //{ "(plusp -0.0s0)", "nil" },
        //{ "(plusp -0.0f0)", "nil" },
        //{ "(plusp -0.0d0)", "nil" },
        //{ "(plusp -0.0l0)", "nil" },

        // Floats
        //{ "(plusp 3.145926536)", "t" },
        //{ "(plusp -1.2345e-8)", "nil" },
        //{ "(plusp 0.0)", "nil" },
        //{ fmt.Sprintf("(plusp %g)", math.MaxFloat32), "t" },
        //{ fmt.Sprintf("(plusp %g)", math.MaxFloat64), "t" },
        //{ fmt.Sprintf("(plusp %g)", math.SmallestNonzeroFloat32), "t" },
        //{ fmt.Sprintf("(plusp %g)", math.SmallestNonzeroFloat64), "t" },

        { "(plusp 'a)", "Eval error: plusp argument needs to be number" },
        { "(plusp)", "Eval error: plusp expecting an argument" },
        { "(plusp 1 0)", "Eval error: plusp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_minusp)
{
    auto fmt = boost::format("(minusp %1%)");
    vector<TestEval> tests = {
        //{ "(minusp (- 2 3))", "t" },
        { "(minusp -1)", "t" },
        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "nil" },

        { "(minusp 0)", "nil" },
        { "(minusp 0)", "nil" },
        { "(minusp 0)", "nil" },
        //{ "(minusp 0.0)", "nil" },
        //{ "(minusp 0.0s0)", "nil" },
        //{ "(minusp 0.0f0)", "nil" },
        //{ "(minusp 0.0d0)", "nil" },
        //{ "(minusp 0.0l0)", "nil" },
        //{ "(minusp -0.0)", "nil" },
        //{ "(minusp -0.0s0)", "nil" },
        //{ "(minusp -0.0f0)", "nil" },
        //{ "(minusp -0.0d0)", "nil" },
        //{ "(minusp -0.0l0)", "nil" },

        { "(minusp 1)", "nil" },

        // Floats
        //{ "(minusp 3.145926536)", "nil" },
        //{ "(minusp -1.2345e-8)", "t" },
        //{ "(minusp 0.0)", "nil" },
        //{ fmt.Sprintf("(minusp %g)", -math.MaxFloat32), "t" },
        //{ fmt.Sprintf("(minusp %g)", -math.MaxFloat64), "t" },
        //{ fmt.Sprintf("(minusp %g)", -math.SmallestNonzeroFloat32), "t" },
        //{ fmt.Sprintf("(minusp %g)", -math.SmallestNonzeroFloat64), "t" },

        { "(minusp 'a)", "Eval error: minusp argument needs to be number" },
        { "(minusp)", "Eval error: minusp expecting an argument" },
        { "(minusp 1 0)", "Eval error: minusp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_evenp)
{
    auto fmt = boost::format("(evenp %1%)");
    vector<TestEval> tests = {
        { "(evenp 1)", "nil" },
        //{ "(evenp (+ 3 3))", "t" },
        { "(evenp 0)", "t" },
        { "(evenp -1)", "nil" },

        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "nil" },
        { boost::str(fmt % numeric_limits<long>::max()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "t" },

        //{ "(evenp 3.145926536)", "Eval error: evenp argument needs to be integer\nnil" },
        { "(evenp 'a)", "Eval error: evenp argument needs to be number" },
        { "(evenp)", "Eval error: evenp expecting an argument" },
        { "(evenp 1 0)", "Eval error: evenp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_oddp)
{
    auto fmt = boost::format("(oddp %1%)");
    vector<TestEval> tests = {
        { "(oddp 1)", "t" },
        //{ "(oddp (/ 2 3))", "nil" },
        { "(oddp 0)", "nil" },
        { "(oddp -1)", "t" },

        { boost::str(fmt % numeric_limits<long>::min()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "t" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "nil" },

        //{ "(oddp 3.145926536)", "Error: oddp argument needs to be integer\nnil" },
        { "(oddp 'a)", "Eval error: oddp argument needs to be number" },
        { "(oddp)", "Eval error: oddp expecting an argument" },
        { "(oddp 1 0)", "Eval error: oddp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_equal)
{
    auto fmt = boost::format("(= %1% %1%)");
    vector<TestEval> tests = {
        { "(= 1 1)", "t" },
        { "(= 1 2)", "nil" },
        { "(= -34 -34)", "t" },
        { "(= 0 -0)", "t" },

        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "t" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "t" },

        // Floats
        // { "(= 1.5 1.5)", "t" },
        // { "(= 1.5 3.145926536)", "nil" },
        // { "(= 1 3.145926536)", "nil" },
        // { "(= 3.145926536 1)", "nil" },
        // { "(= 0.0 0.0)", "t" },
        // { "(= 0.0 -0.0)", "t" },

        // { fmt.Sprintf("(= %[1]g %[1]g)", math.MaxFloat32), "t" },
        // { fmt.Sprintf("(= %[1]g %[1]g)", math.MaxFloat64), "t" },
        // { fmt.Sprintf("(= %[1]g %[1]g)", math.SmallestNonzeroFloat32), "t" },
        // { fmt.Sprintf("(= %[1]g %[1]g)", math.SmallestNonzeroFloat64), "t" },

        // // Mixed
        // { "(= 0 0.0)", "t" },
        // { "(= 0 0.0s0)", "t" },
        // { "(= 0.0f0 0.0s0)", "t" },

        // { "(= 17 17.0)", "t" },
        // { "(= 17 17.0s0)", "t" },
        // { "(= 17.0f0 17.0d0)", "t" },

        { "(= 's 0)", "Eval error: = arguments needs to be number" },
        { "(= 234 'q)", "Eval error: = arguments needs to be number" },
        { "(=)", "Eval error: = expecting 2 arguments" },
        { "(= 1)", "Eval error: = expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_less)
{
    auto fmt = boost::format("(<= %1% %1%)");
    vector<TestEval> tests = {
        { "(< 1 2)", "t" },
        { "(< 2 1)", "nil" },
        { "(<= 1 2)", "t" },
        { "(<= 1 1)", "t" },
        { "(<= 2 1)", "nil" },

        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "t" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "t" },

        // Floats
        // { "(< 1.5 1.5)", "nil" },
        // { "(<= 1.5 1.5)", "t" },
        // { "(< 1.5 3.145926536)", "t" },
        // { "(< 1 3.145926536)", "t" },
        // { "(< 3.145926536 1)", "nil" },

        // { fmt.Sprintf("(<= %[1]g %[1]g)", math.MaxFloat32), "t" },
        // { fmt.Sprintf("(<= %[1]g %[1]g)", math.MaxFloat64), "t" },
        // { fmt.Sprintf("(<= %[1]g %[1]g)", math.SmallestNonzeroFloat32), "t" },
        // { fmt.Sprintf("(<= %[1]g %[1]g)", math.SmallestNonzeroFloat64), "t" },

        // { "(<= 0 0.0)", "t" },
        // { "(<= 0 0.0s0)", "t" },
        // { "(<= 0.0f0 0.0s0)", "t" },

        // { "(<= 17 17.0)", "t" },
        // { "(<= 17 17.0s0)", "t" },
        // { "(<= 17.0f0 17.0d0)", "t" },

        { "(< 's 0)", "Eval error: < arguments needs to be number" },
        { "(<= 234 'dois)", "Eval error: <= arguments needs to be number" },
        { "(<)", "Eval error: < expecting 2 arguments" },
        { "(<= 1)", "Eval error: <= expecting 2 arguments" }
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_greater)
{
    auto fmt = boost::format("(> %1% %1%)");
    vector<TestEval> tests = {
        { "(> 1 2)", "nil" },
        { "(> 2 1)", "t" },
        { "(>= 1 2)", "nil" },
        { "(>= 1 1)", "t" },
        { "(>= 2 1)", "t" },

        { boost::str(fmt % numeric_limits<long>::min()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "nil" },
        { boost::str(fmt % numeric_limits<long>::max()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "nil" },

        // Floats
        // { "(> 1.5 1.5)", "nil" },
        // { "(>= 1.5 1.5)", "t" },
        // { "(> 1.5 3.145926536)", "nil" },
        // { "(> 1 3.145926536)", "nil" },
        // { "(> 3.145926536 1)", "t" },

        // { fmt.Sprintf("(>= %[1]g %[1]g)", math.MaxFloat32), "t" },
        // { fmt.Sprintf("(>= %[1]g %[1]g)", math.MaxFloat64), "t" },
        // { fmt.Sprintf("(>= %[1]g %[1]g)", math.SmallestNonzeroFloat32), "t" },
        // { fmt.Sprintf("(>= %[1]g %[1]g)", math.SmallestNonzeroFloat64), "t" },

        { "(> 's 0)", "Eval error: > arguments needs to be number" },
        { "(>= 234 'dois)", "Eval error: >= arguments needs to be number" },
        { "(>)", "Eval error: > expecting 2 arguments" },
        { "(>= 1)", "Eval error: >= expecting 2 arguments" }
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_notequals)
{
    auto fmt = boost::format("(/= %1% %1%)");
    vector<TestEval> tests = {
        { "(/= 1 1)", "nil" },
        { "(/= 1 2)", "t" },
        { "(/= -34 -34)", "nil" },
        { "(/= 0 -0)", "nil" },

        { boost::str(fmt % numeric_limits<long>::min()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "nil" },
        { boost::str(fmt % numeric_limits<long>::max()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "nil" },

        // Floats
        // { "(/= 1.5 1.5)", "nil" },
        // { "(/= 1.5 3.145926536)", "t" },
        // { "(/= 1 3.145926536)", "t" },
        // { "(/= 3.145926536 1)", "t" },

        // { fmt.Sprintf("(/= %[1]g %[1]g)", math.MaxFloat32), "nil" },
        // { fmt.Sprintf("(/= %[1]g %[1]g)", math.MaxFloat64), "nil" },
        // { fmt.Sprintf("(/= %[1]g %[1]g)", math.SmallestNonzeroFloat32), "nil" },
        // { fmt.Sprintf("(/= %[1]g %[1]g)", math.SmallestNonzeroFloat64), "nil" },

        { "(/= 's 0)", "Eval error: /= arguments needs to be number" },
        { "(/= 234 'dois)", "Eval error: /= arguments needs to be number" },
        { "(/=)", "Eval error: /= expecting 2 arguments" },
        { "(/= 1)", "Eval error: /= expecting 2 arguments" }

    };
    test_Evaluator(tests);
}
