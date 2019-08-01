//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE expr_test
#include <boost/test/unit_test.hpp>
#include <sstream>

#include "expr.hh"

using namespace ax;
using namespace std;

BOOST_AUTO_TEST_CASE(expr_is)
{
    // Test Atoms
    Expr a = Atom("hello");
    BOOST_TEST(is_Atom(a) == true);
    BOOST_TEST(is_Int(a) == false);
    BOOST_TEST(is_List(a) == false);

    Expr b = Int(1);
    BOOST_TEST(is_Atom(b) == false);
    BOOST_TEST(is_Int(b) == true);
    BOOST_TEST(is_List(b) == false);

    Expr c = List({ Atom("hello"), Int(1) });
    BOOST_TEST(is_Atom(c) == false);
    BOOST_TEST(is_Int(c) == false);
    BOOST_TEST(is_List(c) == true);

    Expr d = nullptr; // Can take any value
    BOOST_TEST(is_Atom(d) == false);
    BOOST_TEST(is_Int(d) == false);
    BOOST_TEST(is_List(d) == false);
}

BOOST_AUTO_TEST_CASE(expr_as)
{
    Expr c = List({ Atom("hello"), Int(1) });
    BOOST_TEST(as_List(c).size() == size_t(2));
}

BOOST_AUTO_TEST_CASE(expr_print)
{
    stringstream ss;
    Expr a = List();
    ss << a;
    BOOST_REQUIRE_EQUAL(ss.str(), "()");

    Expr c = List({ Atom("hello"), Int(1) });
    (ss = stringstream()) << c;
    BOOST_REQUIRE_EQUAL(ss.str(), "(hello 1)");

    Expr d = List({ Atom("hello"), Int(1), c });
    (ss = stringstream()) << d;
    BOOST_REQUIRE_EQUAL(ss.str(), "(hello 1 (hello 1))");
}