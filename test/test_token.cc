//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE token_test
#include <boost/test/unit_test.hpp>
#include <sstream>

#include "token.hh"

using namespace ax;
using namespace std;

BOOST_AUTO_TEST_CASE(token_out)
{
    // Test Atoms
    Token a = Token(TokenType::open);
    stringstream ss;
    ss << a;
    BOOST_REQUIRE_EQUAL(ss.str(), "(");

    a = Token(TokenType::atom, "hello"s);
    (ss = stringstream()) << a;
    BOOST_REQUIRE_EQUAL(ss.str(), "hello");
}