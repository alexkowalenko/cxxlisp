//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_lexer
#include <boost/test/unit_test.hpp>
#include <sstream>
#include <vector>

#include "exceptions.hh"
#include "lexer.hh"
#include "linereaderStream.hh"

using namespace ax;
using namespace std;

struct TestLexer {
    string input;
    TokenType tok;
    string atom;
};

void test_Lexer(const vector<TestLexer>& tests);

BOOST_AUTO_TEST_CASE(test_lexer_1)
{
    vector<TestLexer> tests = {
        { "(", TokenType::open, "" },
        { ")", TokenType::close, "" },
        { "a", TokenType::atom, "a" },
        { "jones ", TokenType::atom, "jones" },
        { "a-if", TokenType::atom, "a-if" },

        // Ints
        { "1", TokenType::atom, "1" },
        { "2345", TokenType::atom, "2345" },
        { "84598798479", TokenType::atom, "84598798479" },
        { "0", TokenType::atom, "0" },
        { "-1", TokenType::atom, "-1" },
        { ":", TokenType::atom, ":" },

        { "lambda", TokenType::atom, "lambda" }, // Example identifers from R3RS
        { "list->vector", TokenType::atom, "list->vector" },
        { "+", TokenType::atom, "+" },
        { "<=?", TokenType::atom, "<=?" },
        { "q", TokenType::atom, "q" },
        { "V17a", TokenType::atom, "V17a" },
        { "a34kTMNs", TokenType::atom, "a34kTMNs" },
        { "the-word-recursion-has-many-meanings", TokenType::atom, "the-word-recursion-has-many-meanings" },

        // unicode identifiers
        { "estação", TokenType::atom, "estação" },
        { "λὀγος", TokenType::atom, "λὀγος" },
        { "ἄλφα", TokenType::atom, "ἄλφα" },
        { "👾", TokenType::atom, "👾" },

        { "format.^.\\:{.1", TokenType::atom, "format.^.\\:{.1" }, // In common lisp tests

        { "+", TokenType::atom, "+" }, // R4RS Identifiers in r4rstest.scm
        { "-", TokenType::atom, "-" },
        { "...", TokenType::atom, "..." },
        { "!..", TokenType::atom, "!.." },
        { "$.+", TokenType::atom, "$.+" },
        { "%.-", TokenType::atom, "%.-" },
        { "&.!", TokenType::atom, "&.!" },
        { "*.:", TokenType::atom, "*.:" },
        { "/:.", TokenType::atom, "/:." },
        { ":+.", TokenType::atom, ":+." },
        { "<-.", TokenType::atom, "<-." },
        { "=.", TokenType::atom, "=." },
        { ">.", TokenType::atom, ">." },
        { "?.", TokenType::atom, "?." },
        { "~.", TokenType::atom, "~." },
        { "_.", TokenType::atom, "_." },
        { "^.", TokenType::atom, "^." },

        { ".", TokenType::dot, "." },
        { ".1", TokenType::atom, ".1" }, // wrong
        { "!=", TokenType::atom, "!=" },
        { "'", TokenType::quote, "" },

        { "`", TokenType::backquote, "" },
        { ",", TokenType::comma, "," },
        { "@", TokenType::at, "@" },
        { "#", TokenType::hash, "" },
    };

    test_Lexer(tests);
}

BOOST_AUTO_TEST_CASE(test_lexer_2)
{
    vector<TestLexer> tests = {
        { "", TokenType::eof, "" },
        { ";", TokenType::eof, "" },
        { "; jones", TokenType::eof, "" },
    };

    test_Lexer(tests);
}

void test_Lexer(const vector<TestLexer>& tests)
{
    for (auto test : tests) {
        istringstream is(test.input);
        LineReaderStream r(is);
        Lexer lex(r);
        try {
            auto tok = lex.get_token();
            cout << "type " << tok.type << " wanted " << test.tok << endl;
            BOOST_REQUIRE_EQUAL(tok.type, test.tok);
            if (tok.type == TokenType::atom || tok.type == TokenType::string) {
                cout << "  atom " << tok.val << " wanted " << test.atom << endl;
                BOOST_REQUIRE_EQUAL(tok.val, test.atom);
            }
        } catch (exception& e) {
            BOOST_FAIL("Exception thrown!");
        }
    }
}