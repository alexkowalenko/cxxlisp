//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "test.hh"

#include <gtest/gtest.h>

TEST(eval, strings) {
    std::vector<TestEval> tests = {
        {R"("one two three")", R"("one two three")"},
        {R"("ἑν δύο τρεῖς")", R"("ἑν δύο τρεῖς")"},
        {R"("一二三四五六七")", R"("一二三四五六七")"},
        {R"("🍏🍎🍐🍊🍋🍌🍉🍇🍓🍈🍒")", R"("🍏🍎🍐🍊🍋🍌🍉🍇🍓🍈🍒")"},
        {R"("")", R"("")"},
    };
    test_Evaluator(tests);
}

TEST(eval, stringp) {
    std::vector<TestEval> tests = {
        {R"( (stringp "one two three") )", "t"},
        {R"((stringp "ἑν δύο τρεῖς"))", "t"},
        {R"((stringp ""))", "t"},

        {R"((stringp 0))", "nil"},
        {R"((stringp t))", "nil"},
        {R"((stringp 'a))", "nil"},
        {R"((stringp '(a b c)))", "nil"},
        {R"((stringp "one \"two\" three"))", "t"},
        {R"((stringp))", "Eval error: stringp expecting an argument"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_eq) {
    std::vector<TestEval> tests = {
        {R"((string= "one" "two three"))", "nil"},
        {R"((string-equal "one" "one"))", "t"},
        {R"((string= "one" 1))", "Eval error: string= arguments needs to be a string"},
        {R"((string-equal "" ""))", "t"},
        {R"((string= "one"))", "Eval error: string= expecting 2 arguments"},
        {R"((string-equal))", "Eval error: string-equal expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_neq) {
    std::vector<TestEval> tests = {
        {R"((string/= "one"
                     "two three"))",
         "t"},
        {R"((string-not-equal "one" "one"))", "nil"},
        {R"((string/= "one" 1))", "Eval error: string/= arguments needs to be a string"},
        {R"((string-not-equal "" ""))", "nil"},
        {R"((string/= "one"))", "Eval error: string/= expecting 2 arguments"},
        {R"((string-not-equal))", "Eval error: string-not-equal expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_lt) {
    std::vector<TestEval> tests = {
        {R"((string> "one" "two three"))", "nil"},
        {R"((string-greaterp "one" "one"))", "nil"},
        {R"((string> "ZZZa" "ZZZb"))", "nil"},
        {R"((string-greaterp "one" 1))",
         "Eval error: string-greaterp arguments needs to be a string"},
        {R"((string> "" ""))", "nil"},
        {R"((string-greaterp "one" ))", "Eval error: string-greaterp expecting 2 arguments"},
        {R"((string>))", "Eval error: string> expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_gt) {
    std::vector<TestEval> tests = {
        {R"((string> "one" "two three"))", "nil"},
        {R"((string-greaterp "one" "one"))", "nil"},
        {R"((string> "ZZZa" "ZZZb"))", "nil"},
        {R"((string-greaterp "one" 1))",
         "Eval error: string-greaterp arguments needs to be a string"},
        {R"((string> "" ""))", "nil"},
        {R"((string-greaterp "one" ))", "Eval error: string-greaterp expecting 2 arguments"},
        {R"((string>))", "Eval error: string> expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_le) {
    std::vector<TestEval> tests = {
        {R"((string<= "one" "two three"))", "t"},
        {R"((string-not-greaterp "one" "one"))", "t"},
        {R"((string<= "one" 1))", "Eval error: string<= arguments needs to be a string"},
        {R"((string-not-greaterp "" ""))", "t"},
        {R"((string<= "one" ))", "Eval error: string<= expecting 2 arguments"},
        {R"((string-not-greaterp))", "Eval error: string-not-greaterp expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_ge) {
    std::vector<TestEval> tests = {
        {R"((string>= "one" "two three"))", "nil"},
        {R"((string-not-lessp "one" "one"))", "t"},
        {R"((string>= "one" "ONE"))", "t"},
        {R"((string-not-lessp "one" 1))",
         "Eval error: string-not-lessp arguments needs to be a string"},
        {R"((string>= "" ""))", "t"},
        {R"((string-not-lessp "one" ))", "Eval error: string-not-lessp expecting 2 arguments"},
        {R"((string>=))", "Eval error: string>= expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_eq_ci) {
    std::vector<TestEval> tests = {
        {R"((string-ci= "one" "two three"))", "nil"},
        {R"((string-ci= "one" "one"))", "t"},
        {R"((string-ci= "one" "ONE"))", "t"},
        {R"((string-ci= "OnE" "one"))", "t"},
        {R"((string-ci= "one" 1))", "Eval error: string-ci= arguments needs to be a string"},
        {R"((string-ci= "" ""))", "t"},
        {R"((string-ci= "one" ))", "Eval error: string-ci= expecting 2 arguments"},
        {R"((string-ci=))", "Eval error: string-ci= expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_lt_ci) {
    std::vector<TestEval> tests = {
        {R"((string-ci< "one" "two three"))", "t"},
        {R"((string-ci< "one" "ONE"))", "nil"},
        {R"((string-ci< "b" "aaa"))", "nil"},
        {R"((string-ci< "one" 1))", "Eval error: string-ci< arguments needs to be a string"},
        {R"((string-ci< "" ""))", "nil"},
        {R"((string-ci< "one" ))", "Eval error: string-ci< expecting 2 arguments"},
        {R"((string-ci<))", "Eval error: string-ci< expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_gt_ci) {
    std::vector<TestEval> tests = {
        {R"((string-ci> "one" "two three"))", "nil"},
        {R"((string-ci> "ONE" "one"))", "nil"},
        {R"((string-ci> "ZZZa" "ZZZb"))", "nil"},
        {R"((string-ci> "one" 1))", "Eval error: string-ci> arguments needs to be a string"},
        {R"((string-ci> "" ""))", "nil"},
        {R"((string-ci> "one" ))", "Eval error: string-ci> expecting 2 arguments"},
        {R"((string-ci>))", "Eval error: string-ci> expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_le_ci) {
    std::vector<TestEval> tests = {
        {R"((string-ci<= "one" "two three"))", "t"},
        {R"((string-ci<= "one" "ONE"))", "t"},
        {R"((string-ci<= "ONE" "one"))", "t"},
        {R"((string-ci<= "one" 1))", "Eval error: string-ci<= arguments needs to be a string"},
        {R"((string-ci<= "" ""))", "t"},
        {R"((string-ci<= "one" ))", "Eval error: string-ci<= expecting 2 arguments"},
        {R"((string-ci<=))", "Eval error: string-ci<= expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_ge_ci) {
    std::vector<TestEval> tests = {
        {R"((string-ci>= "one" "two three"))", "nil"},
        {R"((string-ci>= "ONE" "one"))", "t"},
        {R"((string-ci>= "one" "ONE"))", "t"},
        {R"((string-ci>= "one" 1))", "Eval error: string-ci>= arguments needs to be a string"},
        {R"((string-ci>= "" ""))", "t"},
        {R"((string-ci>= "one" ))", "Eval error: string-ci>= expecting 2 arguments"},
        {R"((string-ci>=))", "Eval error: string-ci>= expecting 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, string_string) {
    std::vector<TestEval> tests = {
        {R"((string "one"))", R"("one")"},
        {R"((string 'one))", R"("one")"},
        {R"((string #\a))", R"("a")"},
        {R"((string 123))", R"("123")"},

        // { R"((string-capitalize "one"))", R"("One")" },
        // { R"((string-capitalize 'one))", R"("One")" },
        // { R"((string-capitalize #\a))", R"("A")" },

        {R"((string-upcase "one"))", R"("ONE")"},
        {R"((string-upcase 'one))", R"("ONE")"},
        {R"((string-upcase #\a))", R"("A")"},

        {R"((string-downcase "one"))", R"("one")"},
        {R"((string-downcase 'oNe))", R"("one")"},
        {R"((string-downcase #\a))", R"("a")"},

        {R"((string))", "Eval error: string expecting an argument"},
    };
    test_Evaluator(tests);
}
