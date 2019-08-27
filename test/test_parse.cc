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
        { "()", "nil" },
        { "(a)", "(a)" },
        { "", "" },

        { "(a b c)", "(a b c)" },
        { "((a) b)", "((a) b)" },
        { "(a (b))", "(a (b))" },
        { "(a (b) c)", "(a (b) c)" },
        { "(a (b) (a (b) c))", "(a (b) (a (b) c))" },
        { "(a (b) (a (b) (a (b) (a (b) c))))", "(a (b) (a (b) (a (b) (a (b) c))))" },
        { "(a (b) (a (b) (a (b) (a (b) (a (b) (a (b) (a (b) (a (b) c))))))))",
            "(a (b) (a (b) (a (b) (a (b) (a (b) (a (b) (a (b) (a (b) c))))))))" },

        { "(a (b))", "(a (b))" },
        { "(a ((a (b))))", "(a ((a (b))))" },
        { "(a ((a ((a ((a (b))))))))", "(a ((a ((a ((a (b))))))))" },

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

BOOST_AUTO_TEST_CASE(test_parser_multiline_comments)
{
    vector<TestParser> tests = {
        { "a #| Hello |#", "a" },
        { "#| Hello |# a", "a" },
        { R"(#|
			 
			In 1960, John McCarthy published a remarkable paper in which he did 
			for programming something like what Euclid did for geometry.

		  |#)",
            "" },
        { "a #||#", "a" },
        { "(a #| Hello |# b c)", "(a b c)" },
        { R"(#|

            |#a)",
            "a" },
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
        { "'a", "'a" },
        { "('a 'b 'c)", "('a 'b 'c)" },
        { "('a '(b c))", "('a '(b c))" },
        { "('(a b) 'c)", "('(a b) 'c)" },
    };
    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_backquote)
{
    vector<TestParser> tests = {
        // // backquote
        { "`a", "(backquote a)" },
        { "(`a `b `c)", "((backquote a) (backquote b) (backquote c))" },
        { "(`a `(b c))", "((backquote a) (backquote (b c)))" },
        { "(`(a b) `c)", "((backquote (a b)) (backquote c))" },
        // // unquote
        { ",", "unquote" },
        { "`(cons x ,a)", "(backquote (cons x unquote a))" },
        // // splice-unquote
        { ",@", "splice-unquote" },
        { "`(cons x ,@ a)", "(backquote (cons x splice-unquote a))" },
    };
    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_unicode)
{
    vector<TestParser> tests = {
        { "七", "七" },
        { "(一 二 三)", "(一 二 三)" },
        { "(liberté (égalité fraternité))",
            "(liberté (égalité fraternité))" },
    };
    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_numbers)
{
    vector<TestParser> tests = {
        { "1", "1" },
        { "0", "0" },
        { "46846368464", "46846368464" },
        { "-1", "-1" },
        //{ "+1", "1" },
        { to_string(numeric_limits<long>::min()), "-9223372036854775808" },
        { to_string(numeric_limits<long>::min() + 1), "-9223372036854775807" },
        { to_string(numeric_limits<long>::max()), "9223372036854775807" },
        { to_string(numeric_limits<long>::max() - 1), "9223372036854775806" },

        { "(42 7)", "(42 7)" },

        // Floats
        { "1.2", "1.2" },
        { "0.6", "0.6" },
        { "-46846368.464", "-46846368.464" },
        { "3.145926536", "3.145926536" },
        //{ "1.2345e-8", "1.2345e-08" },
        //{ "-1.0", "-1" },
        //{ "+1.0", "1" },
    };
    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_atoms)
{
    vector<TestParser> tests = {
        // unicode and emoji
        { "one", "one" },
        { "κὀσμος", "κὀσμος" },
        { "👾", "👾" },
        { "🍊🍎🍌", "🍊🍎🍌" },
        { "😀", "😀" },
    };
    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_strings)
{
    vector<TestParser> tests = {
        // { R"("one")", R"("one")" },
        // { R"("κὀσμος")", R"("κὀσμος")" },

        // { R"("👾")", R"("👾")" },
        // { R"("🇵🇹")", R"("🇵🇹")" },
        // { R"("🏄🏻‍🏖")", R"("🏄🏻‍🏖")" },
        // { R"("")", R"("")" },
    };
    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_char)
{
    vector<TestParser> tests = {
        // { "#\\a", "#\\a" },
        // { "#\\1", "#\\1" },
        // { "#\\A", "#\\A" },
        // { "#\\.", "#\\." },
        // { "#\\;", "#\\;" },
        // { "#\\(", "#\\(" },
        // { "#\\)", "#\\)" },
        // { "#\\#", "#\\#" },
        // { "#\\\\", "#\\\\" },
        // { "#\\ἄ", "#\\ἄ" },
        // { "#\\七", "#\\七" },
        // { "#\\👾", "#\\👾" },
        // { "#\\space", "#\\space" },
        // { "#\\newline", "#\\newline" },
        // { "#\\SPACE", "#\\space" },
        // { "#\\NeWlInE", "#\\newline" },

        // { "(#\\A #\\ἄ #\\七)", "(#\\A #\\ἄ #\\七)" },

        // { "#\\abc", "#\\a" }, // this is not really correct
    };
    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_functionrefs)
{
    vector<TestParser> tests = {
        // { "#'id", "#'id" },
        // { "#'+", "#'+" },
    };
    test_Parser(tests);
}

BOOST_AUTO_TEST_CASE(test_parser_keyword)
{
    vector<TestParser> tests = {
        // { ":keyword", ":keyword" },
        // { "&keyword", "&keyword" },
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
            outStr << to_string(val);
            cout << boost::format("parse %1% : %2%") % test.input % outStr.str() << endl;
            if (test.output != outStr.str()) {
                BOOST_ERROR(boost::format("%1% should \nbe: %3%, \nnot %2%") % test.input % outStr.str() % test.output);
            }
        } catch (UnknownToken& e) {
            BOOST_ERROR("Unknown token: " << e.tok);
        } catch (ParseException& e) {
            BOOST_ERROR("Parse error: " << e.what());
        } catch (EOFException&) {
            BOOST_ERROR("EOF: ");
        } catch (exception& e) {
            BOOST_ERROR(boost::format("Exception thrown %1%") % e.what());
        } catch (...) {
            BOOST_ERROR("Unknown exception thrown on : " << test.input);
        }
    }
}
