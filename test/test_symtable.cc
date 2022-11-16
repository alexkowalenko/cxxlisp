//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <gtest/gtest.h>

#include <iostream>

#include "symboltable.hh"

using namespace ax;

TEST(symboltable, invoker) {
    SymbolTable tab = mk_symbol_table();
    tab->put("a", mk_atom("hello"));
    tab->put("b", mk_int(3));
    tab->put("c", sF);

    if (auto x = tab->find("a")) {
        std::cout << to_string(*x) << " ";
        EXPECT_EQ(to_string(*x), "hello");
    } else {
        std::cout << "a not found";
        FAIL();
    }

    if (auto x = tab->find("b")) {
        std::cout << to_string(*x) << " ";
        EXPECT_EQ(to_string(*x), "3");
    } else {
        std::cout << "b not found";
        FAIL();
    }

    if (auto x = tab->find("c")) {
        std::cout << to_string(*x) << " ";
        EXPECT_EQ(to_string(*x), "nil");
    } else {
        std::cout << "c not found";
        FAIL();
    }

    if (auto x = tab->find("d")) {
        std::cout << "d should not be found";
    };

    auto aa = mk_atom("aa");
    auto aaa = mk_atom("aa");
    tab->put(aa->atom, aa);
    if (auto x = tab->find(aaa->atom)) {
        std::cout << to_string(*x) << std::endl;
        EXPECT_EQ(to_string(*x), "aa");
    } else {
        std::cout << "a not found";
        FAIL();
    }
}

TEST(symboltable, nested) {
    SymbolTable tab = mk_symbol_table();

    tab->put("a", mk_atom("hello"));
    tab->put("b", mk_int(3));
    tab->put("c", sF);

    auto tab2 = mk_symbol_table(tab);
    tab2->put("d", mk_atom("Bonjour"));

    if (auto x = tab2->find("a")) {
        std::cout << to_string(*x) << " ";
        EXPECT_EQ(to_string(*x), "hello");
    } else {
        std::cout << "a not found";
        FAIL();
    }

    if (auto x = tab2->find("b")) {
        std::cout << to_string(*x) << " ";
        EXPECT_EQ(to_string(*x), "3");
    } else {
        std::cout << "b not found";
        FAIL();
    }

    if (auto x = tab2->find("c")) {
        std::cout << to_string(*x) << " ";
        EXPECT_EQ(to_string(*x), "nil");
    } else {
        std::cout << "c not found";
        FAIL();
    }

    if (auto x = tab2->find("d")) {
        std::cout << to_string(*x) << std::endl;
        EXPECT_EQ(to_string(*x), "Bonjour");
    } else {
        std::cout << "d should not be found";
        FAIL();
    };
}

TEST(symboltable, set) {
    SymbolTable tab = mk_symbol_table();

    tab->put("a", mk_atom("hello"));
    tab->put("b", mk_int(3));
    tab->put("c", sF);

    SymbolTable tab2 = mk_symbol_table(tab);
    tab2->put("d", mk_atom("bonjour"));

    if (auto x = tab2->find("a")) {
        std::cout << to_string(*x) << " ";
        EXPECT_EQ(to_string(*x), "hello");
    } else {
        std::cout << "a not found";
        FAIL();
    }

    tab2->set("a", mk_atom("bonjour"));

    //  Find in second
    if (auto x = tab2->find("a")) {
        std::cout << to_string(*x) << " ";
        EXPECT_EQ(to_string(*x), "bonjour");
    } else {
        std::cout << "a not found";
        FAIL();
    }

    // Find in first
    if (auto x = tab->find("a")) {
        std::cout << to_string(*x) << " ";
        EXPECT_EQ(to_string(*x), "bonjour");
    } else {
        std::cout << "a not found";
        FAIL();
    }
}
