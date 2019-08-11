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
    BOOST_TEST(is_a<Atom>(a) == true);
    BOOST_TEST(is_a<Int>(a) == false);
    BOOST_TEST(is_a<List>(a) == false);
    BOOST_TEST(is_a<nullptr_t>(a) == false);

    Expr b = Int(1);
    BOOST_TEST(is_a<Atom>(b) == false);
    BOOST_TEST(is_a<Int>(b) == true);
    BOOST_TEST(is_a<List>(b) == false);
    BOOST_TEST(is_a<nullptr_t>(b) == false);

    Expr c = List({ Atom("hello"), Int(1) });
    BOOST_TEST(is_a<Atom>(c) == false);
    BOOST_TEST(is_a<Int>(c) == false);
    BOOST_TEST(is_a<List>(c) == true);
    BOOST_TEST(is_a<nullptr_t>(c) == false);

    Expr d = nullptr; // Can take any value
    BOOST_TEST(is_a<Atom>(d) == false);
    BOOST_TEST(is_a<Int>(d) == false);
    BOOST_TEST(is_a<List>(d) == false);
    BOOST_TEST(is_a<nullptr_t>(d) == true);
}

BOOST_AUTO_TEST_CASE(expr_as)
{
    Expr c = List({ Atom("hello"), Int(1) });
    BOOST_TEST(any_cast<List>(c).size() == size_t(2));
}

BOOST_AUTO_TEST_CASE(expr_print)
{
    stringstream ss;
    Expr a = List();
    ss << a;
    BOOST_REQUIRE_EQUAL(ss.str(), "nil");

    Expr c = List({ Atom("hello"), Int(1) });
    (ss = stringstream()) << c;
    BOOST_REQUIRE_EQUAL(ss.str(), "(hello 1)");

    Expr d = List({ Atom("hello"), Int(1), c });
    (ss = stringstream()) << d;
    BOOST_REQUIRE_EQUAL(ss.str(), "(hello 1 (hello 1))");
}