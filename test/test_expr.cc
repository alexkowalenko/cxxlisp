//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <gtest/gtest.h>
#include <sstream>

#include "expr.hh"
#include "function.hh"

using namespace ax;
using namespace std;

TEST(expr, is) {
    // Test Atoms
    Expr a = mk_atom("hello");
    EXPECT_TRUE(is_atom(a) == true);
    EXPECT_TRUE(is_a<Type::atom>(a) == true);
    EXPECT_TRUE(is_list(a) == false);
    EXPECT_TRUE(is_a<Type::list>(a) == false);
    EXPECT_TRUE(is_int(a) == false);

    a = mk_bool(true);
    EXPECT_TRUE(is_atom(a) == false);
    EXPECT_TRUE(is_a<Type::boolean>(a) == true);
    EXPECT_TRUE(is_list(a) == false);
    EXPECT_TRUE(is_bool(a) == true);
    EXPECT_TRUE(is_int(a) == false);

    a = mk_int(7);
    EXPECT_TRUE(is_atom(a) == false);
    EXPECT_TRUE(is_list(a) == false);
    EXPECT_TRUE(is_a<Type::atom>(a) == false);
    EXPECT_TRUE(is_list(a) == false);
    EXPECT_TRUE(is_a<Type::list>(a) == false);
    EXPECT_TRUE(is_int(a) == true);

    Expr b = mk_list();
    EXPECT_TRUE(is_atom(b) == false);
    EXPECT_TRUE(is_list(b) == true);
    EXPECT_TRUE(is_a<Type::atom>(b) == false);
    EXPECT_TRUE(is_list(b) == true);
    EXPECT_TRUE(is_a<Type::list>(b) == true);
    EXPECT_TRUE(is_int(b) == false);

    b = mk_list({mk_atom("hello"), mk_int(1)});
    EXPECT_TRUE(is_atom(b) == false);
    EXPECT_TRUE(is_list(b) == true);
    EXPECT_TRUE(is_bool(b) == false);
    EXPECT_TRUE(is_a<Type::atom>(b) == false);
    EXPECT_TRUE(is_list(b) == true);
    EXPECT_TRUE(is_a<Type::list>(b) == true);
    EXPECT_TRUE(is_int(b) == false);

    Expr e = mk_function_ref("atom");
    EXPECT_TRUE(is_a<Type::atom>(e) == false);
    EXPECT_TRUE(is_a<Type::function_ref>(e) == true);

    Expr f = mk_string(L"Olá!");
    EXPECT_TRUE(is_a<Type::atom>(f) == false);
    EXPECT_TRUE(is_a<Type::string>(f) == true);

    Expr g = mk_char('c');
    EXPECT_TRUE(is_a<Type::atom>(g) == false);
    EXPECT_TRUE(is_a<Type::integer>(g) == false);
    EXPECT_TRUE(is_a<Type::character>(g) == true);
}

TEST(expr, print) {
    stringstream ss;
    Expr        a = mk_list();
    EXPECT_EQ(to_string(a), "nil");
    ss << a;
    EXPECT_EQ(ss.str(), "nil");

    a = mk_atom("hello");
    EXPECT_EQ(to_string(a), "hello");
    (ss = stringstream()) << a;
    EXPECT_EQ(ss.str(), "hello");

    a = mk_int(7);
    EXPECT_EQ(to_string(a), "7");
    (ss = stringstream()) << a;
    EXPECT_EQ(ss.str(), "7");

    a = mk_int(-616561564165146146);
    EXPECT_EQ(to_string(a), "-616561564165146146");
    (ss = stringstream()) << a;
    EXPECT_EQ(ss.str(), "-616561564165146146");

    Expr c = mk_list(mk_atom("hello"));
    EXPECT_EQ(to_string(c), "(hello)");
    (ss = stringstream()) << c;
    EXPECT_EQ(ss.str(), "(hello)");

    c = mk_list(mk_atom("hello"), mk_atom("there"));
    EXPECT_EQ(to_string(c), "(hello . there)");
    (ss = stringstream()) << c;
    EXPECT_EQ(ss.str(), "(hello . there)");

    c = mk_list(mk_atom("hello"), mk_list(mk_atom("there")));
    EXPECT_EQ(to_string(c), "(hello there)");
    (ss = stringstream()) << c;
    EXPECT_EQ(ss.str(), "(hello there)");
    EXPECT_EQ(c->size(), 2u);

    c = mk_list({mk_atom("hello"), mk_atom("there")});
    EXPECT_EQ(to_string(c), "(hello there)");
    (ss = stringstream()) << c;
    EXPECT_EQ(ss.str(), "(hello there)");
    EXPECT_EQ(c->size(), 2u);

    c = mk_list({mk_atom("hello"), mk_atom("there"), mk_atom("jim")});
    EXPECT_EQ(to_string(c), "(hello there jim)");
    (ss = stringstream()) << c;
    EXPECT_EQ(ss.str(), "(hello there jim)");
    EXPECT_EQ(c->size(), 3u);

    c = mk_list({mk_atom("hello"), mk_atom("there"), mk_atom("jim"),
                 mk_list({mk_atom("hello"), mk_atom("there"), mk_atom("jim")})});
    EXPECT_EQ(to_string(c), "(hello there jim (hello there jim))");
    (ss = stringstream()) << c;
    EXPECT_EQ(ss.str(), "(hello there jim (hello there jim))");
    EXPECT_EQ(c->size(), 4u);

    c = mk_list(
        {mk_list({mk_atom("hello"), mk_list({mk_atom("hello"), mk_atom("there"), mk_atom("jim")}),
                  mk_atom("there"), mk_atom("jim")}),
         mk_atom("hello"), mk_atom("there"), mk_atom("jim"),
         mk_list({mk_atom("hello"), mk_atom("there"), mk_atom("jim")})});
    EXPECT_EQ(to_string(c), "((hello (hello there jim) there jim) hello "
                            "there jim (hello there jim))");
    (ss = stringstream()) << c;
    EXPECT_EQ(ss.str(), "((hello (hello there jim) there jim) hello "
                        "there jim (hello there jim))");
    EXPECT_EQ(c->size(), 5u);

    Expr f = mk_string(L"Olá!"s);
    EXPECT_EQ(to_string(f), "\"Olá!\"");
    (ss = stringstream()) << f;
    EXPECT_EQ(ss.str(), "\"Olá!\"");

    Expr g = mk_char('g');
    EXPECT_EQ(to_string(g), "#\\g");
    (ss = stringstream()) << g;
    EXPECT_EQ(ss.str(), "#\\g");

    Expr g1 = mk_char(' ');
    EXPECT_EQ(to_string(g1), "#\\space");
    (ss = stringstream()) << g1;
    EXPECT_EQ(ss.str(), "#\\space");

    Expr g2 = mk_char('\n');
    EXPECT_EQ(to_string(g2), "#\\newline");
    (ss = stringstream()) << g2;
    EXPECT_EQ(ss.str(), "#\\newline");

    Expr g3 = mk_char(u'Ж');
    EXPECT_EQ(to_string(g3), "#\\Ж");
    (ss = stringstream()) << g3;
    EXPECT_EQ(ss.str(), "#\\Ж");

    Expr g4 = mk_char(U'👾'); // or L'👾'
    EXPECT_EQ(to_string(g4), "#\\👾");
    (ss = stringstream()) << g4;
    EXPECT_EQ(ss.str(), "#\\👾");
}

TEST(expr_if, False) {
    Expr a = mk_atom("hello");
    EXPECT_TRUE(!is_false(a));

    EXPECT_TRUE(is_false(sF));
    EXPECT_TRUE(is_false(nullptr));
    EXPECT_TRUE(is_false(mk_list()));
}

TEST(expr_eq, test) {
    EXPECT_TRUE(expr_eq(sF, sF) == sT);
    EXPECT_TRUE(expr_eq(sT, sT) == sT);

    EXPECT_TRUE(expr_eq(sT, sF) == sF);
    EXPECT_TRUE(expr_eq(sF, sT) == sF);

    EXPECT_TRUE(expr_eq(mk_atom("hello"), mk_atom("hello")) == sT);
    EXPECT_TRUE(expr_eq(mk_atom("hello"), mk_atom("olá")) == sF);

    EXPECT_TRUE(expr_eq(mk_int(7), mk_int(7)) == sT);
    EXPECT_TRUE(expr_eq(mk_int(7), mk_int(-13)) == sF);

    EXPECT_TRUE(expr_eq(sT, mk_atom("hello")) == sF);
    EXPECT_TRUE(expr_eq(mk_atom("hello"), mk_int(-13)) == sF);
    EXPECT_TRUE(expr_eq(mk_int(7), sT) == sF);

    EXPECT_TRUE(expr_eq(mk_list(), mk_list()) == sT);

    EXPECT_TRUE(expr_equal(mk_list(), mk_list()) == sT);
    EXPECT_TRUE(expr_equal(mk_list(mk_atom("a")), mk_list(mk_atom("a"))) == sT);
    EXPECT_TRUE(expr_equal(mk_list(mk_atom("a")), mk_list(mk_atom("b"))) == sF);

    EXPECT_TRUE(expr_equal(mk_list(mk_atom("a")), mk_list({mk_atom("a"), mk_atom("b")})) == sF);
    EXPECT_TRUE(expr_equal(mk_list({mk_atom("a"), mk_atom("b")}), mk_list(mk_atom("a"))) == sF);

    EXPECT_TRUE(expr_equal(mk_list({mk_atom("a"), mk_list({mk_atom("a"), mk_atom("b")})}),
                           mk_list({mk_atom("a"), mk_list({mk_atom("a"), mk_atom("b")})})) == sT);

    EXPECT_TRUE(expr_equal(mk_list({mk_atom("a"), mk_list({mk_atom("a"), mk_atom("b")})}),
                           mk_list({mk_atom("a"), mk_list({mk_atom("a"), mk_atom("c")})})) == sF);
}

TEST(expr, index) {
    auto c = mk_list({mk_atom("hello"), mk_atom("there"), mk_atom("jim")});
    EXPECT_EQ(to_string((*c)[0]), "hello");
    EXPECT_EQ(to_string((*c)[1]), "there");
    EXPECT_EQ(to_string((*c)[2]), "jim");
    EXPECT_EQ(to_string((*c)[3]), "nil");
}

TEST(expr, set) {
    auto c = mk_list({mk_atom("hello"), mk_atom("there"), mk_atom("jim")});
    c->set(0, mk_atom("bonjour"));
    EXPECT_EQ(to_string(c), "(bonjour there jim)");
    c->set(1, mk_atom("lá"));
    EXPECT_EQ(to_string(c), "(bonjour lá jim)");
    c->set(2, mk_atom("jacques"));
    EXPECT_EQ(to_string(c), "(bonjour lá jacques)");
}