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

BOOST_AUTO_TEST_CASE(expr_is)
{
    // Test Atoms
    Expr* a = mk_atom("hello");
    BOOST_TEST(is_atom(a) == true);
    BOOST_TEST(is_a<Type::atom>(a) == true);
    BOOST_TEST(is_list(a) == false);
    BOOST_TEST(is_a<Type::list>(a) == false);
    BOOST_TEST(is_int(a) == false);

    a = mk_bool(true);
    BOOST_TEST(is_atom(a) == false);
    BOOST_TEST(is_a<Type::boolean>(a) == true);
    BOOST_TEST(is_list(a) == false);
    BOOST_TEST(is_bool(a) == true);
    BOOST_TEST(is_int(a) == false);

    a = mk_int(7);
    BOOST_TEST(is_atom(a) == false);
    BOOST_TEST(is_list(a) == false);
    BOOST_TEST(is_a<Type::atom>(a) == false);
    BOOST_TEST(is_list(a) == false);
    BOOST_TEST(is_a<Type::list>(a) == false);
    BOOST_TEST(is_int(a) == true);

    Expr* b = mk_list();
    BOOST_TEST(is_atom(b) == false);
    BOOST_TEST(is_list(b) == true);
    BOOST_TEST(is_a<Type::atom>(b) == false);
    BOOST_TEST(is_list(b) == true);
    BOOST_TEST(is_a<Type::list>(b) == true);
    BOOST_TEST(is_int(b) == false);

    b = mk_list({ mk_atom("hello"), mk_int(1) });
    BOOST_TEST(is_atom(b) == false);
    BOOST_TEST(is_list(b) == true);
    BOOST_TEST(is_bool(b) == false);
    BOOST_TEST(is_a<Type::atom>(b) == false);
    BOOST_TEST(is_list(b) == true);
    BOOST_TEST(is_a<Type::list>(b) == true);
    BOOST_TEST(is_int(b) == false);

    // Expr e = FunctionRef("atom");
    // BOOST_TEST(is_a<Atom>(e) == false);
    // BOOST_TEST(is_a<FunctionRef>(e) == true);

    // Expr f = String(L"Olá!");
    // BOOST_TEST(is_a<Atom>(f) == false);
    // BOOST_TEST(is_a<String>(f) == true);

    // Expr g = Char('c');
    // BOOST_TEST(is_a<Atom>(g) == false);
    // BOOST_TEST(is_a<Int>(g) == false);
    // BOOST_TEST(is_a<Char>(g) == true);
}

BOOST_AUTO_TEST_CASE(expr_print)
{
    stringstream ss;
    Expr* a = mk_list();
    BOOST_CHECK_EQUAL(to_string(a), "nil");
    ss << a;
    BOOST_CHECK_EQUAL(ss.str(), "nil");

    a = mk_atom("hello");
    BOOST_CHECK_EQUAL(to_string(a), "hello");
    (ss = stringstream()) << a;
    BOOST_CHECK_EQUAL(ss.str(), "hello");

    a = mk_int(7);
    BOOST_CHECK_EQUAL(to_string(a), "7");
    (ss = stringstream()) << a;
    BOOST_CHECK_EQUAL(ss.str(), "7");

    a = mk_int(-616561564165146146);
    BOOST_CHECK_EQUAL(to_string(a), "-616561564165146146");
    (ss = stringstream()) << a;
    BOOST_CHECK_EQUAL(ss.str(), "-616561564165146146");

    Expr* c = mk_list(mk_atom("hello"));
    BOOST_CHECK_EQUAL(to_string(c), "(hello)");
    (ss = stringstream()) << c;
    BOOST_CHECK_EQUAL(ss.str(), "(hello)");

    c = mk_list(mk_atom("hello"), mk_atom("there"));
    BOOST_CHECK_EQUAL(to_string(c), "(hello . there)");
    (ss = stringstream()) << c;
    BOOST_CHECK_EQUAL(ss.str(), "(hello . there)");

    c = mk_list(mk_atom("hello"), mk_list(mk_atom("there")));
    BOOST_CHECK_EQUAL(to_string(c), "(hello there)");
    (ss = stringstream()) << c;
    BOOST_CHECK_EQUAL(ss.str(), "(hello there)");
    BOOST_CHECK_EQUAL(size_list(c), 2);

    c = mk_list({ mk_atom("hello"), mk_atom("there") });
    BOOST_CHECK_EQUAL(to_string(c), "(hello there)");
    (ss = stringstream()) << c;
    BOOST_CHECK_EQUAL(ss.str(), "(hello there)");
    BOOST_CHECK_EQUAL(size_list(c), 2);

    c = mk_list({ mk_atom("hello"), mk_atom("there"), mk_atom("jim") });
    BOOST_CHECK_EQUAL(to_string(c), "(hello there jim)");
    (ss = stringstream()) << c;
    BOOST_CHECK_EQUAL(ss.str(), "(hello there jim)");
    BOOST_CHECK_EQUAL(size_list(c), 3);

    c = mk_list({ mk_atom("hello"), mk_atom("there"), mk_atom("jim"), mk_list({ mk_atom("hello"), mk_atom("there"), mk_atom("jim") }) });
    BOOST_CHECK_EQUAL(to_string(c), "(hello there jim (hello there jim))");
    (ss = stringstream()) << c;
    BOOST_CHECK_EQUAL(ss.str(), "(hello there jim (hello there jim))");
    BOOST_CHECK_EQUAL(size_list(c), 4);

    c = mk_list({ mk_list({ mk_atom("hello"), mk_list({ mk_atom("hello"), mk_atom("there"), mk_atom("jim") }), mk_atom("there"), mk_atom("jim") }), mk_atom("hello"), mk_atom("there"), mk_atom("jim"), mk_list({ mk_atom("hello"), mk_atom("there"), mk_atom("jim") }) });
    BOOST_CHECK_EQUAL(to_string(c), "((hello (hello there jim) there jim) hello there jim (hello there jim))");
    (ss = stringstream()) << c;
    BOOST_CHECK_EQUAL(ss.str(), "((hello (hello there jim) there jim) hello there jim (hello there jim))");
    BOOST_CHECK_EQUAL(size_list(c), 5);

    // Expr f = String(L"Olá!"s);
    // BOOST_CHECK_EQUAL(to_string(f), "\"Olá!\"");
    // (ss = stringstream()) << f;
    // BOOST_CHECK_EQUAL(ss.str(), "\"Olá!\"");

    // Expr g = Char('g');
    // BOOST_CHECK_EQUAL(to_string(g), "#\\g");
    // (ss = stringstream()) << g;
    // BOOST_CHECK_EQUAL(ss.str(), "#\\g");

    // Expr g1 = Char(' ');
    // BOOST_CHECK_EQUAL(to_string(g1), "#\\space");
    // (ss = stringstream()) << g1;
    // BOOST_CHECK_EQUAL(ss.str(), "#\\space");

    // Expr g2 = Char('\n');
    // BOOST_CHECK_EQUAL(to_string(g2), "#\\newline");
    // (ss = stringstream()) << g2;
    // BOOST_CHECK_EQUAL(ss.str(), "#\\newline");

    // Expr g3 = Char(u'Ж');
    // BOOST_CHECK_EQUAL(to_string(g3), "#\\Ж");
    // (ss = stringstream()) << g3;
    // BOOST_CHECK_EQUAL(ss.str(), "#\\Ж");

    // Expr g4 = Char(U'👾'); // or L'👾'
    // BOOST_CHECK_EQUAL(to_string(g4), "#\\👾");
    // (ss = stringstream()) << g4;
    // BOOST_CHECK_EQUAL(ss.str(), "#\\👾");
}

BOOST_AUTO_TEST_CASE(expr_if_false)
{
    Expr* a = mk_atom("hello");
    BOOST_CHECK(!is_false(a));

    BOOST_CHECK(is_false(sF));
    BOOST_CHECK(is_false(nullptr));
    BOOST_CHECK(is_false(mk_list()));
}

BOOST_AUTO_TEST_CASE(expr_eq_test)
{
    BOOST_CHECK(expr_eq(sF, sF) == sT);
    BOOST_CHECK(expr_eq(sT, sT) == sT);

    BOOST_CHECK(expr_eq(sT, sF) == sF);
    BOOST_CHECK(expr_eq(sF, sT) == sF);

    BOOST_CHECK(expr_eq(mk_atom("hello"), mk_atom("hello")) == sT);
    BOOST_CHECK(expr_eq(mk_atom("hello"), mk_atom("olá")) == sF);

    BOOST_CHECK(expr_eq(mk_int(7), mk_int(7)) == sT);
    BOOST_CHECK(expr_eq(mk_int(7), mk_int(-13)) == sF);

    BOOST_CHECK(expr_eq(sT, mk_atom("hello")) == sF);
    BOOST_CHECK(expr_eq(mk_atom("hello"), mk_int(-13)) == sF);
    BOOST_CHECK(expr_eq(mk_int(7), sT) == sF);

    BOOST_CHECK(expr_eq(mk_list(), mk_list()) == sT);

    BOOST_CHECK(expr_equal(mk_list(), mk_list()) == sT);
    BOOST_CHECK(expr_equal(mk_list(mk_atom("a")), mk_list(mk_atom("a"))) == sT);
    BOOST_CHECK(expr_equal(mk_list(mk_atom("a")), mk_list(mk_atom("b"))) == sF);

    BOOST_CHECK(expr_equal(mk_list(mk_atom("a")), mk_list({ mk_atom("a"), mk_atom("b") })) == sF);
    BOOST_CHECK(expr_equal(mk_list({ mk_atom("a"), mk_atom("b") }), mk_list(mk_atom("a"))) == sF);

    BOOST_CHECK(expr_equal(mk_list({ mk_atom("a"), mk_list({ mk_atom("a"), mk_atom("b") }) }),
                    mk_list({ mk_atom("a"), mk_list({ mk_atom("a"), mk_atom("b") }) }))
        == sT);

    BOOST_CHECK(expr_equal(mk_list({ mk_atom("a"), mk_list({ mk_atom("a"), mk_atom("b") }) }),
                    mk_list({ mk_atom("a"), mk_list({ mk_atom("a"), mk_atom("c") }) }))
        == sF);
}