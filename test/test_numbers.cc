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

using namespace std;

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

        // floats
        { "1.2", "1.2" },
        { "0.6", "0.6" },
        { "-46846368.464", "-46846368.464" },
        { "3.145926536", "3.145926536" },
        { "1.2345e-8", "1.2345e-08" },
        { "-1.0", "-1" },
        { "+1.0", "1" },

        // complex
        { "#C(1 2)", "#c(1 2)" },
        { "#C(-1 2)", "#c(-1 2)" },
        { "#C(0 0)", "#c(0 0)" },
        { "#C(0.5 -0.25)", "#c(0.5 -0.25)" },
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
        { "(numberp (+ 2 3))", "t" },
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
        { "(integerp (+ 2 3))", "t" },
        { "(integerp 3.145926536)", "nil" },
        //{ "(integerp #C(1 2))", "nil" },
        { "(integerp 'a)", "nil" },
        { "(integerp '(a b c))", "nil" },
        { "(integerp t)", "nil" },
        { "(integerp nil)", "nil" },

        { "(integerp)", "Eval error: integerp expecting an argument" },
        { "(integerp nil nil)", "Eval error: integerp expecting an argument" },

        // realp
        { "(realp 1)", "nil" },
        { "(realp (+ 2 3))", "nil" },
        { "(realp 0)", "nil" },
        //{ "(realp #C(1 2))", "nil" },
        { "(realp 'a)", "nil" },
        { "(realp '(a b c))", "nil" },
        { "(realp t)", "nil" },
        { "(realp nil)", "nil" },

        // floatp
        { "(floatp 1)", "nil" },
        { "(floatp (+ 2 3))", "nil" },
        { "(floatp 0)", "nil" },
        //{ "(floatp #C(1 2))", "nil" },
        { "(floatp 'a)", "nil" },
        { "(floatp '(a b c))", "nil" },
        { "(floatp t)", "nil" },
        { "(floatp nil)", "nil" },

        // // floats
        { "(numberp 3.145926536)", "t" },
        { "(numberp -46846368.464)", "t" },
        { "(numberp 1.2345e-8)", "t" },

        { "(integerp 1)", "t" },
        { "(integerp -46846368.464)", "nil" },
        { "(integerp 1.2345e-8)", "nil" },

        { "(realp 3.145926536)", "t" },
        { "(realp -46846368.464)", "t" },
        { "(realp -1.2345e-8)", "t" },

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
        { "(zerop (+ 2 3))", "nil" },
        { "(zerop -1)", "nil" },
        { boost::str(fmt % numeric_limits<long>::min()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "nil" },
        { boost::str(fmt % numeric_limits<long>::max()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "nil" },

        // Floats
        { "(zerop 3.145926536)", "nil" },
        { "(zerop -1.2345e-8)", "nil" },
        { boost::str(fmt % numeric_limits<double>::min()), "Numeric exception: float out of range 2.22507e-308\nParse error: Extra bracket found" },
        { boost::str(fmt % numeric_limits<double>::max()), "nil" },
        { boost::str(fmt % numeric_limits<double>::epsilon()), "nil" },
        { boost::str(fmt % numeric_limits<double>::lowest()), "nil" },

        // Complex
        //{ "(zerop #C(0 0))", "t" },
        //{ "(zerop #C(0 1))", "nil" },

        { "(zerop 'a)", "Eval error: zerop argument needs to be a number" },
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
        { "(plusp (* 2 3))", "t" },
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
        { "(plusp 3.145926536)", "t" },
        { "(plusp -1.2345e-8)", "nil" },
        { "(plusp 0.0)", "nil" },
        // { boost::str(fmt % numeric_limits<double>::min()), "t" },
        { boost::str(fmt % numeric_limits<double>::max()), "t" },
        { boost::str(fmt % numeric_limits<double>::epsilon()), "t" },
        { boost::str(fmt % numeric_limits<double>::lowest()), "nil" },

        { "(plusp 'a)", "Eval error: plusp argument needs to be a number" },
        { "(plusp)", "Eval error: plusp expecting an argument" },
        { "(plusp 1 0)", "Eval error: plusp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_minusp)
{
    auto fmt = boost::format("(minusp %1%)");
    vector<TestEval> tests = {
        { "(minusp (- 2 3))", "t" },
        { "(minusp -1)", "t" },
        { "(minusp 1)", "nil" },
        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "nil" },

        { "(minusp 0)", "nil" },
        { "(minusp 0.0)", "nil" },
        //{ "(minusp 0.0s0)", "nil" },
        //{ "(minusp 0.0f0)", "nil" },
        //{ "(minusp 0.0d0)", "nil" },
        //{ "(minusp 0.0l0)", "nil" },
        { "(minusp -0.0)", "nil" },
        //{ "(minusp -0.0s0)", "nil" },
        //{ "(minusp -0.0f0)", "nil" },
        //{ "(minusp -0.0d0)", "nil" },
        //{ "(minusp -0.0l0)", "nil" },

        // Floats
        { "(minusp 3.145926536)", "nil" },
        { "(minusp -1.2345e-8)", "t" },
        { "(minusp 0.0)", "nil" },
        //{ boost::str(fmt % numeric_limits<double>::min()), "nil" },
        { boost::str(fmt % numeric_limits<double>::max()), "nil" },
        { boost::str(fmt % numeric_limits<double>::epsilon()), "nil" },
        { boost::str(fmt % numeric_limits<double>::lowest()), "t" },

        { "(minusp 'a)", "Eval error: minusp argument needs to be a number" },
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
        { "(evenp (+ 3 3))", "t" },
        { "(evenp 0)", "t" },
        { "(evenp -1)", "nil" },

        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "nil" },
        { boost::str(fmt % numeric_limits<long>::max()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "t" },

        { "(evenp 3.145926536)", "Eval error: evenp argument needs to be a integer" },
        { "(evenp 'a)", "Eval error: evenp argument needs to be a integer" },
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
        { "(oddp (/ 2 3))", "nil" },
        { "(oddp 0)", "nil" },
        { "(oddp -1)", "t" },

        { boost::str(fmt % numeric_limits<long>::min()), "nil" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "t" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "nil" },

        { "(oddp 3.145926536)", "Eval error: oddp argument needs to be a integer" },
        { "(oddp 'a)", "Eval error: oddp argument needs to be a integer" },
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
        { "(= 1.5 1.5)", "t" },
        { "(= 1.5 3.145926536)", "nil" },
        { "(= 1 3.145926536)", "nil" },
        { "(= 3.145926536 1)", "nil" },
        { "(= 0.0 0.0)", "t" },
        { "(= 0.0 -0.0)", "t" },

        //{ boost::str(fmt % numeric_limits<double>::min()), "t" },
        { boost::str(fmt % numeric_limits<double>::max()), "t" },
        { boost::str(fmt % numeric_limits<double>::epsilon()), "t" },
        { boost::str(fmt % numeric_limits<double>::lowest()), "t" },

        // Mixed
        { "(= 0 0.0)", "t" },
        //{ "(= 0 0.0s0)", "t" },
        //{ "(= 0.0f0 0.0s0)", "t" },

        { "(= 17 17.0)", "t" },
        //{ "(= 17 17.0s0)", "t" },
        //{ "(= 17.0f0 17.0d0)", "t" },

        { "(= 's 0)", "Eval error: = arguments needs to be a number" },
        { "(= 234 'q)", "Eval error: = arguments needs to be a number" },
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
        { "(< 1.5 1.5)", "nil" },
        { "(<= 1.5 1.5)", "t" },
        { "(< 1.5 3.145926536)", "t" },
        { "(< 1 3.145926536)", "t" },
        { "(< 3.145926536 1)", "nil" },

        //{ boost::str(fmt % numeric_limits<double>::min()), "t" },
        { boost::str(fmt % numeric_limits<double>::max()), "t" },
        { boost::str(fmt % numeric_limits<double>::epsilon()), "t" },
        { boost::str(fmt % numeric_limits<double>::lowest()), "t" },

        { "(<= 0 0.0)", "t" },
        // { "(<= 0 0.0s0)", "t" },
        // { "(<= 0.0f0 0.0s0)", "t" },

        { "(<= 17 17.0)", "t" },
        // { "(<= 17 17.0s0)", "t" },
        // { "(<= 17.0f0 17.0d0)", "t" },

        { "(< 's 0)", "Eval error: < arguments needs to be a number" },
        { "(<= 234 'dois)", "Eval error: <= arguments needs to be a number" },
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
        { "(> 1.5 1.5)", "nil" },
        { "(>= 1.5 1.5)", "t" },
        { "(> 1.5 3.145926536)", "nil" },
        { "(> 1 3.145926536)", "nil" },
        { "(> 3.145926536 1)", "t" },

        //{ boost::str(fmt % numeric_limits<double>::min()), "nil" },
        { boost::str(fmt % numeric_limits<double>::max()), "nil" },
        { boost::str(fmt % numeric_limits<double>::epsilon()), "nil" },
        { boost::str(fmt % numeric_limits<double>::lowest()), "nil" },

        { "(> 's 0)", "Eval error: > arguments needs to be a number" },
        { "(>= 234 'dois)", "Eval error: >= arguments needs to be a number" },
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
        { "(/= 1.5 1.5)", "nil" },
        { "(/= 1.5 3.145926536)", "t" },
        { "(/= 1 3.145926536)", "t" },
        { "(/= 3.145926536 1)", "t" },

        //{ boost::str(fmt % numeric_limits<double>::min()), "nil" },
        { boost::str(fmt % numeric_limits<double>::max()), "nil" },
        { boost::str(fmt % numeric_limits<double>::epsilon()), "nil" },
        { boost::str(fmt % numeric_limits<double>::lowest()), "nil" },

        { "(/= 's 0)", "Eval error: /= arguments needs to be a number" },
        { "(/= 234 'dois)", "Eval error: /= arguments needs to be a number" },
        { "(/=)", "Eval error: /= expecting 2 arguments" },
        { "(/= 1)", "Eval error: /= expecting 2 arguments" }

    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_add)
{
    vector<TestEval> tests = {
        { "(+ 12)", "12" },
        { "(+ 2 3)", "5" },
        { "(+ 3 -3)", "0" },
        { "(+ 0 0)", "0" },
        { "(+ 0 2)", "2" },
        { "(+ 1 2 3 4 5 6)", "21" },
        { "(+)", "0" },

        // floats
        { "(+ 1.2)", "1.2" },
        { "(+ 2.1 3)", "5.1" },
        { "(+ 2 3.1)", "5.1" },
        { "(+ 2.1 3.1)", "5.2" },
        { "(+ 2.1 3.1 1.1)", "6.3" },

        // Mixed
        { "(+ 2.1 3)", "5.1" },
        { "(+ 2 3.1)", "5.1" },

        // // complex
        // { "(+ 1 #C(0 1))", "#C(1 1)" },
        // { "(+ #C(0 1) 1.5)", "#C(1.5 1)" },

        { "(+ 1 'jones)", "Eval error: + arguments needs to be a number" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_mult)
{
    vector<TestEval> tests = {
        { "(* 12)", "12" },
        { "(* 2 3)", "6" },
        { "(* 3 -3)", "-9" },
        { "(* 0 0)", "0" },
        { "(* 0 2)", "0" },
        { "(* 1 2 3 4 5 6)", "720" },
        { "(* 66433534 345847684)", "22975883873835256" },
        { "(*)", "1" },

        // floats
        { "(* 1.2)", "1.2" },
        { "(* 2.1 3)", "6.3" },
        { "(* 2 3.1)", "6.2" },
        { "(* 2.1 3.1)", "6.51" },
        { "(* 2.1 3.1 1.1)", "7.161" },

        //{ boost::str(boost::format("(* %1% 0)") % numeric_limits<double>::min()), "0" },
        { boost::str(boost::format("(* 0 %1%)") % numeric_limits<double>::max()), "0" },
        { boost::str(boost::format("(* %1% 0)") % numeric_limits<double>::epsilon()), "0" },
        { boost::str(boost::format("(* 0 %1%)") % numeric_limits<double>::lowest()), "-0" },

        // Complex
        //{ "(* #C(0 1) #C(0 1))", "#C(-1 0)" },

        { "(* 1 'jones)", "Eval error: * arguments needs to be a number" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_sub)
{
    vector<TestEval> tests = {
        { "(- 12)", "-12" },
        { "(- 2 3)", "-1" },
        { "(- 3 -3)", "6" },
        { "(- 0 0)", "0" },
        { "(- 0 2)", "-2" },
        { "(- 2)", "-2" },

        // floats
        { "(- 1.2)", "-1.2" },
        { "(- 2.1 3)", "-0.9" },
        { "(- 2 3.1)", "-1.1" },
        { "(- 2.1 3.1)", "-1" },
        { "(- 2.1 3.1 1.1)", "-2.1" },

        // // Complex
        // { "(- #C(1 3) #C(3 1))", "#C(-2 2)" },

        { "(- 1 'jones)", "Eval error: - arguments needs to be a number" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_div)
{
    vector<TestEval> tests = {
        { "(/ 32 2)", "16" },
        { "(/ 32 -3)", "-10" },
        { "(/ 0 0)", "Numeric exception: divide by zero" },
        { "(/ 0 2)", "0" },
        { "(/ 3 -1)", "-3" },
        { "(/ 66433345534 34684)", "1915388" },

        { "(/ 1 'jones)", "Eval error: / arguments needs to be a number" },

        // floats
        { "(/ 0.5 2)", "0.25" },
        { "(/ 1 3.0)", "0.333333333333" },
        { "(/ 8.0)", "0.125" },
        { "(/ 3.0 4 5)", "0.15" },

        // // Complex
        // { "(/ #C(1 0) #C(0 1))", "#C(0 -1)" },

        { "(/ )", "Eval error: / expecting at least 1 arguments" },

        // mod
        { "(mod 32 2)", "0" },
        { "(rem 32 -3)", "2" },
        { "(mod 0 0)", "Numeric exception: divide by zero" },
        { "(rem 0 2)", "0" },
        { "(mod 3 -1)", "0" },
        { "(mod 66433345534 28142)", "17660" },
        { "(mod 12)", "Eval error: mod expecting 2 arguments" },
        { "(mod 1 'jones)", "Eval error: mod arguments needs to be a number" },

        { "(mod 13 4)", "1" },
        { "(rem 13 4)", "1" },
        //{ "(mod -13 4)", "3" },
        { "(rem -13 4)", "-1" },
        //{ "(mod 13 -4)", "-3" },
        { "(rem 13 -4)", "1" },
        { "(mod -13 -4)", "-1" },
        { "(rem -13 -4)", "-1" },

        //{ "(mod 13.4 1)", "0.4" },
        //{ "(rem 13.4 1)", "0.4" },
        //{ "(mod -13.4 1)", "0.6" },
        //{ "(rem -13.4 1)", "-0.4" },

    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_power)
{
    vector<TestEval> tests = {
        { "(^ 12)", "12" },
        { "(expt 2 3)", "8" },
        { "(^ 3 -3)", "0.037037037037" },
        { "(^ 3 -3.0)", "0.037037037037" },
        { "(expt 0 0)", "1" },
        { "(^ 0 2)", "0" },

        { "(^ 1.1 1.1)", "1.11053424105" },

        // Complex
        //{ "(^ #C(0 1) #C(0 1))", "#C(0.20787957635076193 0)" },

        { "(^ 1 'jones)", "Eval error: ^ arguments needs to be a number" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_max)
{
    vector<TestEval> tests = {
        { "(max 12)", "12" },
        { "(max 12 24)", "24" },
        { "(max 3 -3)", "3" },
        { "(max 0 0)", "0" },
        { "(max 0 2)", "2" },
        { "(max 1 2 3 4 5 6)", "6" },
        { "(max 66433534 345847684)", "345847684" },
        { "(max 2)", "2" },

        { "(max 1.2 2.4)", "2.4" },

        { "(max 1 'jones)", "Eval error: max arguments needs to be a number" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_min)
{
    vector<TestEval> tests = {
        { "(min 12)", "12" },
        { "(min 12 24)", "12" },
        { "(min 3 -3)", "-3" },
        { "(min 0 0)", "0" },
        { "(min 0 2)", "0" },
        { "(min 1 2 3 4 5 6)", "1" },
        { "(min 66433534 345847684)", "66433534" },
        { "(min 2)", "2" },

        { "(min 1.2 2.4)", "1.2" },

        { "(min 1 'jones)", "Eval error: min arguments needs to be a number" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_abs)
{
    vector<TestEval> tests = {
        { "(abs 12)", "12" },
        { "(abs -12)", "12" },
        { "(abs 0)", "0" },

        { "(abs)", "Eval error: abs expecting an argument" },
        { "(abs 'jones)", "Eval error: abs argument needs to be a number" },

        { "(floor 12)", "12" },
        { "(ceiling 12)", "12" },
        { "(truncate 12)", "12" },
        { "(round 12)", "12" },

        // floats
        { "(abs 12.0)", "12" },
        { "(abs -12.0)", "12" },
        { "(abs 0.0)", "0" },

        { "(floor 12.7)", "12" },
        { "(ceiling 12.7)", "13" },
        { "(truncate 12.7)", "12" },
        { "(round 12.7)", "13" },

        { "(floor -12.7)", "-13" },
        { "(ceiling -12.7)", "-12" },
        { "(truncate -12.7)", "-12" },
        { "(round -12.7)", "-13" },

        { "(floor 0.0)", "0" },
        { "(ceiling 0.0)", "0" },
        { "(truncate 0.0)", "0" },
        { "(round 0.0)", "0" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_real_funcs)
{
    vector<TestEval> tests = {
        { "(log 1.2)", "0.182321556794" },
        { "(exp 1.2)", "3.32011692274" },
        { "(sin 0)", "0" },
        { "(cos 1)", "0.540302305868" },
        { "(tan 1)", "1.55740772465" },
        { "(asin 1)", "1.57079632679" },
        { "(acos 1)", "0" },
        { "(atan 1.2)", "0.876058050598" },
        { "(sqrt 2)", "1.41421356237" },
        { "(sqrt 1)", "1" },
        { "pi", "3.14159265359" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_real_incf)
{
    vector<TestEval> tests = {
        { "(defvar x 1)", "x" },
        { "(incf x)", "2" },
        { "(incf x)", "3" },
        { "(incf x 3)", "6" },
        { "(incf x)", "7" },
        { "x", "7" },

        { "(decf x)", "6" },
        { "(decf x 2)", "4" },
        { "(decf x)", "3" },
        { "(decf x 7)", "-4" },
        { "x", "-4" },

        { "(defvar x 1.2)", "x" },
        { "(incf x)", "2.2" },
        { "(incf x)", "3.2" },
        { "(incf x 3)", "6.2" },
        { "(incf x)", "7.2" },
        { "x", "7.2" },

        { "(decf x)", "6.2" },
        { "(decf x 2.2)", "4" },
        { "(decf x)", "3" },
        { "(decf x 7)", "-4" },
        { "x", "-4" },

        { "(incf)", "Eval error: incf expecting at least 1 arguments" },
        { "(incf 1)", "Eval error: incf: argument needs to a reference" },
        { "(decf y)", "Eval error: decf: undefined variable y" },

        { "(defvar z '(1 2))", "z" },
        { "(incf z)", "Eval error: incf: value is not a number (1 2)" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_real_inc)
{
    vector<TestEval> tests = {
        { "(defvar x 1)", "x" },
        { "(1+ x)", "2" },
        { "x", "1" },

        { "(1- x)", "0" },
        { "x", "1" },

        { "(1- 6)", "5" },
        { "(1- 5.5)", "4.5" },

        { "(1+)", "Eval error: 1+ expecting an argument" },
        { "(1- 'a)", "Eval error: 1- argument needs to be a number" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_float)
{
    vector<TestEval> tests = {
        { "(float 1)", "1" }, // should print as float.
        { "(float -2.2)", "-2.2" },
        { "(/ 1 2)", "0" },
        { "(/ (float 1) 2)", "0.5" },

        { "(float 'a)", "Eval error: float argument needs to be a number" },
        { "(float)", "Eval error: float expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_radix)
{
    vector<TestEval> tests = {
        { "(+ #B1 #B10)", "3" },
        { "(+ #O1 #B10 #B100)", "7" },
        { "(+ #O1 #B10 #B100 #X10)", "23" },
        { "(+ #xcafe #B0)", "51966" },
    };
    test_Evaluator(tests);
}