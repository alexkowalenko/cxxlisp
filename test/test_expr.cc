//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE expr_test
#include <boost/test/unit_test.hpp>
#include <sstream>

#include "expr.hh"
#include "function.hh"

using namespace ax;
using namespace std;

ostream& operator<<(ostream& os, const Expr& s)
{
    return os << to_string(s);
}

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

    Expr e = FunctionRef("atom");
    BOOST_TEST(is_a<Atom>(e) == false);
    BOOST_TEST(is_a<FunctionRef>(e) == true);

    Expr f = String(L"Olá!");
    BOOST_TEST(is_a<Atom>(f) == false);
    BOOST_TEST(is_a<String>(f) == true);

    Expr g = Char('c');
    BOOST_TEST(is_a<Atom>(g) == false);
    BOOST_TEST(is_a<Int>(g) == false);
    BOOST_TEST(is_a<Char>(g) == true);
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
    BOOST_CHECK_EQUAL(to_string(a), "nil");
    ss << a;
    BOOST_CHECK_EQUAL(ss.str(), "nil");

    Expr c = List({ Atom("hello"), Int(1) });
    BOOST_CHECK_EQUAL(to_string(c), "(hello 1)");
    (ss = stringstream()) << c;
    BOOST_CHECK_EQUAL(ss.str(), "(hello 1)");

    Expr d = List({ Atom("hello"), Int(1), c });
    BOOST_CHECK_EQUAL(to_string(d), "(hello 1 (hello 1))");
    (ss = stringstream()) << d;
    BOOST_CHECK_EQUAL(ss.str(), "(hello 1 (hello 1))");

    Expr f = String(L"Olá!"s);
    BOOST_CHECK_EQUAL(to_string(f), "\"Olá!\"");
    (ss = stringstream()) << f;
    BOOST_CHECK_EQUAL(ss.str(), "\"Olá!\"");

    Expr g = Char('g');
    BOOST_CHECK_EQUAL(to_string(g), "#\\g");
    (ss = stringstream()) << g;
    BOOST_CHECK_EQUAL(ss.str(), "#\\g");

    Expr g1 = Char(' ');
    BOOST_CHECK_EQUAL(to_string(g1), "#\\space");
    (ss = stringstream()) << g1;
    BOOST_CHECK_EQUAL(ss.str(), "#\\space");

    Expr g2 = Char('\n');
    BOOST_CHECK_EQUAL(to_string(g2), "#\\newline");
    (ss = stringstream()) << g2;
    BOOST_CHECK_EQUAL(ss.str(), "#\\newline");

    Expr g3 = Char(u'Ж');
    BOOST_CHECK_EQUAL(to_string(g3), "#\\Ж");
    (ss = stringstream()) << g3;
    BOOST_CHECK_EQUAL(ss.str(), "#\\Ж");

    Expr g4 = Char(U'👾'); // or L'👾'
    BOOST_CHECK_EQUAL(to_string(g4), "#\\👾");
    (ss = stringstream()) << g4;
    BOOST_CHECK_EQUAL(ss.str(), "#\\👾");
}