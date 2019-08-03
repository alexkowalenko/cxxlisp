//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_parser
#include <boost/format.hpp>
#include <boost/test/unit_test.hpp>
#include <sstream>
#include <vector>

#include "exceptions.hh"
#include "linereaderStream.hh"
#include "parser.hh"

using namespace ax;
using namespace std;

struct TestParser {
    string input;
    string output;
};

void test_Parser(const vector<TestParser>& tests);

BOOST_AUTO_TEST_CASE(test_parser)
{
    vector<TestParser> tests = {
        { "t", "t" },
        { "nil", "nil" },
        { "a", "a" },
        { "(t)", "(t)" },
        //{ "()", "nil" },
        { "(a)", "(a)" },
        { "", "" },

        { "(a b c)", "(a b c)" },
        { "((a) b)", "((a) b)" },
        { "(a (b))", "(a (b))" },
        { "(a (b) c)", "(a (b) c)" },

        { "1234567890", "1234567890" },
        { "( s )", "(s)" },
        { "( s s )", "(s s)" },
        { "((a) b s)", "((a) b s)" },
        { "(a b (c))", "(a b (c))" },
        { "(atom a)", "(atom a)" },

        { R"x((a 
			(b) c))x",
            "(a (b) c)" },
        { R"x((a 
				(b) c))x",
            "(a (b) c)" },
        { R"x((a (b) 
				c))x",
            "(a (b) c)" },
        { R"x((
				a (b) c))x",
            "(a (b) c)" },
        { R"x((a (b
				) c))x",
            "(a (b) c)" },
        { R"x((a (b) c
				))x",
            "(a (b) c)" },
        { R"x((   a   (b) c)
			)x",
            "(a (b) c)" },
        { R"x((
				a 
				(b) 
				c))x",
            "(a (b) c)" },
        { R"x((
				a 
				(
					b
					) 
						c
							 ))x",
            "(a (b) c)" },
        { R"x((       a 
			  (b      ) 
			  c       ))x",
            "(a (b) c)" },
        { R"x((   a   (
				b   )   c
				))x",
            "(a (b) c)" },
    };

    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_comments)
{
    vector<TestParser> tests = {
        { "a ; Hello", "a" },
        { "; In 1960, John McCarthy published a remarkable paper in which he did for programming something like what Euclid did for geometry.", "nil" },
    };

    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_TF)
{
    vector<TestParser> tests = {
        { "t", "t" },
        { "nil", "nil" },
        { "(t)", "(t)" },
        { "(nil)", "(nil)" },

        { "(t t)", "(t t)" },
        { "(nil t)", "(nil t)" },
        { "(t nil)", "(t nil)" },
        { "(nil nil)", "(nil nil)" },

        { "(t t (t))", "(t t (t))" },
        { "(nil t (t))", "(nil t (t))" },
        { "(t nil (t))", "(t nil (t))" },
        { "(nil nil (t))", "(nil nil (t))" },
    };

    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_quote)
{
    vector<TestParser> tests = {
        { "'a", "(quote a)" },
        { "('a 'b 'c)", "((quote a) (quote b) (quote c))" },
        { "('a '(b c))", "((quote a) (quote (b c)))" },
        { "('(a b) 'c)", "((quote (a b)) (quote c))" },
    };

    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_backquote)
{
    vector<TestParser> tests = {
        // backquote
        { "`a", "(backquote a)" },
        { "(`a `b `c)", "((backquote a) (backquote b) (backquote c))" },
        { "(`a `(b c))", "((backquote a) (backquote (b c)))" },
        { "(`(a b) `c)", "((backquote (a b)) (backquote c))" },
        // unquote
        { ",", "unquote" },
        { "`(cons x ,a)", "(backquote (cons x unquote a))" },
        // splice-unquote
        { ",@", "splice-unquote" },
        { "`(cons x ,@ a)", "(backquote (cons x splice-unquote a))" },
    };

    test_Parser(tests);
}

void test_Parser(const vector<TestParser>& tests)
{
    for (auto test : tests) {
        istringstream is(test.input);
        LineReaderStream r(is);
        Lexer lex(r);
        Parser parser(lex);
        try {
            BOOST_TEST_CHECKPOINT(test.input);
            auto [val, eof] = parser.parse();
            if (eof) {
                continue;
            }
            ostringstream outStr;
            outStr << val;
            cout << boost::format("parse %1% : %2%") % test.input % outStr.str() << endl;
            if (test.output != outStr.str()) {
                BOOST_FAIL(boost::format("%1% should be %3%, not %2%") % test.input % outStr.str() % test.output);
            }
        } catch (UnknownToken& e) {
            BOOST_FAIL("Unknown token: " << e.tok);
        } catch (ParseException& e) {
            BOOST_FAIL("Parse error: " << e.what());
        } catch (EOFException&) {
            BOOST_FAIL("EOF: ");
        } catch (exception& e) {
            BOOST_FAIL(boost::format("Exception thrown %1%") % e.what());
        } catch (...) {
            BOOST_FAIL("Unknown exception thrown on : " << test.input);
        }
    }
}
