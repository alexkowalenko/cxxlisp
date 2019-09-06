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
    tab.put("a", mk_atom("hello"));
    tab.put("b", mk_int(3));
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

    auto aa = mk_atom("aa");
    auto aaa = mk_atom("aa");
    tab.put(aa->atom, aa);
    if (auto x = tab.find(aaa->atom)) {
        cout << to_string(*x) << endl;
        BOOST_REQUIRE_EQUAL(to_string(*x), "aa");
    } else {
        BOOST_FAIL("a not found");
    }
}

BOOST_AUTO_TEST_CASE(symboltable_nested)
{
    SymbolTable tab(nullptr);

    tab.put("a", mk_atom("hello"));
    tab.put("b", mk_int(3));
    tab.put("c", sF);

    SymbolTable tab2(&tab);
    tab2.put("d", mk_atom("Bonjour"));

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

BOOST_AUTO_TEST_CASE(symboltable_set)
{
    SymbolTable tab(nullptr);

    tab.put("a", mk_atom("hello"));
    tab.put("b", mk_int(3));
    tab.put("c", sF);

    SymbolTable tab2(&tab);
    tab2.put("d", mk_atom("bonjour"));

    if (auto x = tab2.find("a")) {
        cout << to_string(*x) << " ";
        BOOST_REQUIRE_EQUAL(to_string(*x), "hello");
    } else {
        BOOST_FAIL("a not found");
    }

    tab2.set("a", mk_atom("bonjour"));

    //  Find in second
    if (auto x = tab2.find("a")) {
        cout << to_string(*x) << " ";
        BOOST_REQUIRE_EQUAL(to_string(*x), "bonjour");
    } else {
        BOOST_FAIL("a not found");
    }

    // Find in first
    if (auto x = tab.find("a")) {
        cout << to_string(*x) << " ";
        BOOST_REQUIRE_EQUAL(to_string(*x), "bonjour");
    } else {
        BOOST_FAIL("a not found");
    }
}
