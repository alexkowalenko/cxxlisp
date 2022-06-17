//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <boost/format.hpp>

#define SPDLOG_ACTIVE_LEVEL SPDLOG_LEVEL_DEBUG
#include <spdlog/spdlog.h>

#include <boost/test/unit_test.hpp>
#include <sstream>

#include "exceptions.hh"
#include "linereaderStream.hh"
#include "lisp.hh"

#include "test.hh"

using namespace ax;
using namespace std;

void test_Evaluator(const vector<TestEval>& tests)
{
    Options options;
    options.silent = true;
    options.readline = false;
    options.debug_expr = false;

    spdlog::set_level(spdlog::level::info); // Set global log level to info 
    Lisp lisp(options);
    lisp.init();

    for (auto test : tests) {
        istringstream is(test.input);
        ostringstream out;

        //BOOST_TEST_CHECKPOINT(test.input);
        lisp.repl(is, out);

        string result = out.str();
        result.pop_back(); // chop off \n
        cout << "eval: " << test.input << " -> " << result << endl;
        if (test.output != result) {
            BOOST_ERROR(boost::format("\n%1%\nshould be: %3%, \n      not: %2%") % test.input % result % test.output);
            continue;
        }
    }
}
