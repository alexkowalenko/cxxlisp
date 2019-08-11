//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE symboltable
#include <boost/test/unit_test.hpp>
#include <iostream>

#include "symboltable.hh"

using namespace ax;
using namespace std;

BOOST_AUTO_TEST_CASE(symboltable_invoker)
{
    SymbolTable tab(nullptr);

    tab.put("a", Atom("hello"));
    tab.put("b", Int(3));
    tab.put("c", sF);

    if (auto x = tab.find("a")) {
        cout << to_string(*x) << " ";
        BOOST_REQUIRE_EQUAL(to_string(*x), "hello");
    } else {
        BOOST_FAIL("a not found");
    }

    if (auto x = tab.find("b")) {
        cout << to_string(*x) << " ";
        BOOST_REQUIRE_EQUAL(to_string(*x), "3");
    } else {
        BOOST_FAIL("b not found");
    }

    if (auto x = tab.find("c")) {
        cout << to_string(*x) << " ";
        BOOST_REQUIRE_EQUAL(to_string(*x), "nil");
    } else {
        BOOST_FAIL("c not found");
    }

    if (auto x = tab.find("d")) {
        BOOST_FAIL("d should not be found");
    };

    if (auto x = tab.find("d")) {
        BOOST_FAIL("d should not be found");
    };

    auto aa = Atom("aa");
    auto aaa = Atom("aa");
    tab.put(aa, aa);
    if (auto x = tab.find(aaa)) {
        cout << to_string(*x) << endl;
        BOOST_REQUIRE_EQUAL(to_string(*x), "aa");
    } else {
        BOOST_FAIL("a not found");
    }
}

BOOST_AUTO_TEST_CASE(symboltable_nested)
{
    SymbolTable tab(nullptr);

    tab.put("a", Atom("hello"));
    tab.put("b", Int(3));
    tab.put("c", sF);

    SymbolTable tab2(&tab);
    tab2.put("d", Atom("Bonjour"));

    if (auto x = tab2.find("a")) {
        cout << to_string(*x) << " ";
        BOOST_REQUIRE_EQUAL(to_string(*x), "hello");
    } else {
        BOOST_FAIL("a not found");
    }

    if (auto x = tab2.find("b")) {
        cout << to_string(*x) << " ";
        BOOST_REQUIRE_EQUAL(to_string(*x), "3");
    } else {
        BOOST_FAIL("b not found");
    }

    if (auto x = tab2.find("c")) {
        cout << to_string(*x) << " ";
        BOOST_REQUIRE_EQUAL(to_string(*x), "nil");
    } else {
        BOOST_FAIL("c not found");
    }

    if (auto x = tab2.find("d")) {
        cout << to_string(*x) << endl;
        BOOST_REQUIRE_EQUAL(to_string(*x), "Bonjour");
    } else {
        BOOST_FAIL("d should not be found");
    };
}
