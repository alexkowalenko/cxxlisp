//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <sstream>
#include <vector>

#include <fmt/format.h>
#include <gtest/gtest.h>

#include "exceptions.hh"
#include "linereaderStream.hh"
#include "parser.hh"

using namespace ax;

struct TestParser {
    std::string input;
    std::string output;
};

void test_Parser(const std::vector<TestParser> &tests);

TEST(parser, basic) {
    std::vector<TestParser> tests = {
        {"t", "t"},
        {"nil", "nil"},
        {"a", "a"},
        {"(t)", "(t)"},
        {"()", "nil"},
        {"(a)", "(a)"},
        {"", ""},

        {"(a b c)", "(a b c)"},
        {"((a) b)", "((a) b)"},
        {"(a (b))", "(a (b))"},
        {"(a (b) c)", "(a (b) c)"},
        {"(a (b) (a (b) c))", "(a (b) (a (b) c))"},
        {"(a (b) (a (b) (a (b) (a (b) c))))", "(a (b) (a (b) (a (b) (a (b) c))))"},
        {"(a (b) (a (b) (a (b) (a (b) (a (b) (a (b) (a (b) (a (b) c))))))))",
         "(a (b) (a (b) (a (b) (a (b) (a (b) (a (b) (a (b) (a (b) c))))))))"},

        {"(a (b))", "(a (b))"},
        {"(a ((a (b))))", "(a ((a (b))))"},
        {"(a ((a ((a ((a (b))))))))", "(a ((a ((a ((a (b))))))))"},

        {"1234567890", "1234567890"},
        {"( s )", "(s)"},
        {"( s s )", "(s s)"},
        {"((a) b s)", "((a) b s)"},
        {"(a b (c))", "(a b (c))"},
        {"(atom a)", "(atom a)"},

        {"(1+ a)", "(1+ a)"},
        {"(1- a)", "(1- a)"},

        {R"x((a 
			(b) c))x",
         "(a (b) c)"},
        {R"x((a 
				(b) c))x",
         "(a (b) c)"},
        {R"x((a (b) 
				c))x",
         "(a (b) c)"},
        {R"x((
				a (b) c))x",
         "(a (b) c)"},
        {R"x((a (b
				) c))x",
         "(a (b) c)"},
        {R"x((a (b) c
				))x",
         "(a (b) c)"},
        {R"x((   a   (b) c)
			)x",
         "(a (b) c)"},
        {R"x((
				a 
				(b) 
				c))x",
         "(a (b) c)"},
        {R"x((
				a 
				(
					b
					) 
						c
							 ))x",
         "(a (b) c)"},
        {R"x((       a 
			  (b      ) 
			  c       ))x",
         "(a (b) c)"},
        {R"x((   a   (
				b   )   c
				))x",
         "(a (b) c)"},
    };
    test_Parser(tests);
}

TEST(parser, comments) {
    std::vector<TestParser> tests = {
        {"a ; Hello", "a"},
        {"; In 1960, John McCarthy published a remarkable paper in which he did for programming "
         "something like what Euclid did for geometry.",
         "nil"},
    };
    test_Parser(tests);
}

TEST(parser, multiline_comments) {
    std::vector<TestParser> tests = {
        {"a #| Hello |#", "a"},
        {"#| Hello |# a", "a"},
        {R"(#|
			 
			In 1960, John McCarthy published a remarkable paper in which he did 
			for programming something like what Euclid did for geometry.

		  |#)",
         ""},
        {"a #||#", "a"},
        {"(a #| Hello |# b c)", "(a b c)"},
        {R"(#|

            |#a)",
         "a"},
    };
    test_Parser(tests);
}

TEST(parser, TF) {
    std::vector<TestParser> tests = {
        {"t", "t"},
        {"nil", "nil"},
        {"(t)", "(t)"},
        {"(nil)", "(nil)"},

        {"(t t)", "(t t)"},
        {"(nil t)", "(nil t)"},
        {"(t nil)", "(t nil)"},
        {"(nil nil)", "(nil nil)"},

        {"(t t (t))", "(t t (t))"},
        {"(nil t (t))", "(nil t (t))"},
        {"(t nil (t))", "(t nil (t))"},
        {"(nil nil (t))", "(nil nil (t))"},
    };
    test_Parser(tests);
}

TEST(parser, quote) {
    std::vector<TestParser> tests = {
        {"'a", "'a"},
        {"('a 'b 'c)", "('a 'b 'c)"},
        {"('a '(b c))", "('a '(b c))"},
        {"('(a b) 'c)", "('(a b) 'c)"},
    };
    test_Parser(tests);
}

TEST(parser, backquote) {
    std::vector<TestParser> tests = {
        // // backquote
        {"`a", "(backquote a)"},
        {"(`a `b `c)", "((backquote a) (backquote b) (backquote c))"},
        {"(`a `(b c))", "((backquote a) (backquote (b c)))"},
        {"(`(a b) `c)", "((backquote (a b)) (backquote c))"},
        // // unquote
        {",", "unquote"},
        {"`(cons x ,a)", "(backquote (cons x unquote a))"},
        // // splice-unquote
        {",@", "splice-unquote"},
        {"`(cons x ,@ a)", "(backquote (cons x splice-unquote a))"},
    };
    test_Parser(tests);
}

TEST(parser, dot) {
    std::vector<TestParser> tests = {
        // Dot form
        {"(1 . 2)", "(1 . 2)"},
        {"((1 2) . 3)", "((1 2) . 3)"},
        {"(1 2 . 3)", "(1 2 . 3)"},
        {"(1 2 . (3 4))", "(1 2 3 4)"},
        {"(1 . (2 . (3 . (4))))", "(1 2 3 4)"},
        {"(1 . (2 . (3 . (4 . ()))))", "(1 2 3 4)"},
        {"(1 . (2 . (3 . 4)))", "(1 2 3 . 4)"},

        {"1 . 2", "1"},           // should be an error, but 2 is evaluated next
        {"(1 . 2 3)", "(1 . 2)"}, // should be an error, but anything after 2 is ignored.
        {"(1 . 2 3 4)", "(1 . 2)"},
        {"(1 . 2 3 4 5)", "(1 . 2)"},
        {". 2 3 4 5", "2"}, // should be an error, but the dot is ignored
    };
    test_Parser(tests);
}

TEST(parser, unicode) {
    std::vector<TestParser> tests = {
        {"七", "七"},
        {"(一 二 三)", "(一 二 三)"},
        {"(liberté (égalité fraternité))", "(liberté (égalité fraternité))"},
    };
    test_Parser(tests);
}

TEST(parser, numbers) {
    std::vector<TestParser> tests = {
        {"1", "1"},
        {"0", "0"},
        {"46846368464", "46846368464"},
        {"-1", "-1"},
        {"+1", "1"},
        {std::to_string(std::numeric_limits<long>::min()), "-9223372036854775808"},
        {std::to_string(std::numeric_limits<long>::min() + 1), "-9223372036854775807"},
        {std::to_string(std::numeric_limits<long>::max()), "9223372036854775807"},
        {std::to_string(std::numeric_limits<long>::max() - 1), "9223372036854775806"},

        {"(42 7)", "(42 7)"},

        // Different Radix
        {"#B1", "1"},
        {"#B101", "5"},
        {"#B1111", "15"},

        {"#O1", "1"},
        {"#O101", "65"},
        {"#O1111", "585"},
        {"#O777", "511"},

        {"#X1", "1"},
        {"#X101", "257"},
        {"#X1111", "4369"},
        {"#X777", "1911"},
        {"#Xffff", "65535"},

        {"(#B1 #O1 #X1)", "(1 1 1)"},

        // Floats
        {"1.2", "1.2"},
        {"0.6", "0.6"},
        {"-46846368.464", "-46846368.464"},
        {"3.145926536", "3.145926536"},
        {"1.2345e-8", "1.2345e-08"},
        {"-1.0", "-1"},
        {"+1.0", "1"},
    };
    test_Parser(tests);
}

TEST(parser, atoms) {
    std::vector<TestParser> tests = {
        // unicode and emoji
        {"one", "one"}, {"κὀσμος", "κὀσμος"}, {"👾", "👾"}, {"🍊🍎🍌", "🍊🍎🍌"}, {"😀", "😀"},
    };
    test_Parser(tests);
}

TEST(parser, strings) {
    std::vector<TestParser> tests = {
        {R"("one")", R"("one")"},
        {R"("κὀσμος")", R"("κὀσμος")"},

        {R"("👾")", R"("👾")"},
        {R"("🇵🇹")", R"("🇵🇹")"},
        {R"("🏄🏻‍🏖")", R"("🏄🏻‍🏖")"},
        {R"("")", R"("")"},
    };
    test_Parser(tests);
}

TEST(parser, char) {
    std::vector<TestParser> tests = {
        {"#\\a", "#\\a"},
        {"#\\1", "#\\1"},
        {"#\\A", "#\\A"},
        {"#\\.", "#\\."},
        {"#\\;", "#\\;"},
        {"#\\(", "#\\("},
        {"#\\)", "#\\)"},
        {"#\\#", "#\\#"},
        {"#\\\\", "#\\\\"},
        {"#\\ἄ", "#\\ἄ"},
        {"#\\七", "#\\七"},
        {"#\\👾", "#\\👾"},
        {"#\\space", "#\\space"},
        {"#\\newline", "#\\newline"},
        {"#\\SPACE", "#\\space"},
        {"#\\NeWlInE", "#\\newline"},

        {"(#\\A #\\ἄ #\\七)", "(#\\A #\\ἄ #\\七)"},

        {"#\\abc", "#\\a"}, // this is not really correct
    };
    test_Parser(tests);
}

TEST(parser, functionrefs) {
    std::vector<TestParser> tests = {
        {"#'id", "#'id"},
        {"#'+", "#'+"},
    };
    test_Parser(tests);
}

TEST(parser, keyword) {
    std::vector<TestParser> tests = {
        {":keyword", ":keyword"},
        {"&keyword", "&keyword"},
    };
    test_Parser(tests);
}

TEST(parser, vector) {
    std::vector<TestParser> tests = {
        {"#()", "#()"},
        {"#(a )", "#(a)"},

        {"#(a 1 \"2\" (3 s f) #\\4 nil)", "#(a 1 \"2\" (3 s f) #\\4 nil)"},
        {"#(a 1 \"2\" (3 s f) #\\4 nil #(1 2 3))", "#(a 1 \"2\" (3 s f) #\\4 nil #(1 2 3))"},
        {"#(#(#(1)))", "#(#(#(1)))"},
    };
    test_Parser(tests);
}

TEST(parser, complex) {
    std::vector<TestParser> tests = {
        {"#C(1 2)", "#c(1 2)"},
        {"#C(-1 2)", "#c(-1 2)"},
        {"#C(0 0)", "#c(0 0)"},
        {"#C(0.5 0.25)", "#c(0.5 0.25)"},

        {"(defvar z #C(1 3))", "(defvar z #c(1 3))"},
    };
    test_Parser(tests);
}

void test_Parser(const std::vector<TestParser> &tests) {
    for (auto test : tests) {
        std::istringstream is(test.input);
        LineReaderStream   r(is);
        Lexer              lex(r);
        Parser             parser(lex);
        try {
            // BOOST_TEST_CHECKPOINT(test.input);
            auto [val, eof, dot] = parser.parse();
            if (eof) {
                continue;
            }
            std::ostringstream outStr;
            outStr << ax::to_string(val);
            std::cout << fmt::format("parse {} : {}\n", test.input, outStr.str());
            if (test.output != outStr.str()) {
                std::cout << fmt::format("{0} should \nbe: {2}, \nnot {1}", test.input,
                                         outStr.str(), test.output);
                FAIL();
            }
        } catch (UnknownToken &e) {
            std::cout << "Unknown token: " << e.tok;
            FAIL();
        } catch (ParseException &e) {
            std::cout << "Parse error: " << e.what();
            FAIL();
        } catch (EOFException &) {
            std::cout << "EOF: ";
            FAIL();
        } catch (std::exception &e) {
            std::cout << fmt::format("Exception thrown {}", e.what());
            FAIL();
        } catch (...) {
            std::cout << "Unknown exception thrown on : " << test.input;
            FAIL();
        }
    }
}

TEST(test, atoi) {
    std::string a{"1"};
    EXPECT_EQ(ax::atoi(a), 1);
    a = "12";
    EXPECT_EQ(ax::atoi(a), 12);
    a = "+12";
    EXPECT_EQ(ax::atoi(a), 12);
    a = "-12";
    EXPECT_EQ(ax::atoi(a), -12);
    a = "0";
    EXPECT_EQ(ax::atoi(a), 0);
    a = "-0";
    EXPECT_EQ(ax::atoi(a), 0);
    a = "+0";
    EXPECT_EQ(ax::atoi(a), 0);

    a = "1+";
    try {
        EXPECT_EQ(ax::atoi(a), -12);
    } catch (NotInt) {
    };

    a = "+";
    try {
        EXPECT_EQ(ax::atoi(a), -12);
    } catch (NotInt) {
    };
}
