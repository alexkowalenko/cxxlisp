//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

namespace ax {

//
// String functions
//

static std::function<bool(String, String)> eq_str = std::equal_to<String>();
static std::function<bool(String, String)> neq_str = std::not_equal_to<String>();
static std::function<bool(String, String)> gt_str = std::greater<String>();
static std::function<bool(String, String)> ge_str = std::greater_equal<String>();
static std::function<bool(String, String)> lt_str = std::less<String>();
static std::function<bool(String, String)> le_str = std::less_equal<String>();

PrimBasicFunct str_eq = predicate_str<String>(eq_str);
PrimBasicFunct str_neq = predicate_str<String>(neq_str);
PrimBasicFunct str_gt = predicate_str<String>(gt_str);
PrimBasicFunct str_ge = predicate_str<String>(ge_str);
PrimBasicFunct str_lt = predicate_str<String>(lt_str);
PrimBasicFunct str_le = predicate_str<String>(le_str);

PrimBasicFunct funct_ci(PrimBasicFunct f, std::function<Expr(const Expr &)> trans) {
    return [=](Expr args) -> Expr {
        auto new_list = mk_list();
        auto nl = new_list;
        Expr prev = nullptr;
        while (!is_false(args)) {
            nl->car = trans(args->car);
            nl->cdr = mk_list();
            prev = nl;
            nl = nl->cdr;
            args = args->cdr;
        }
        prev->cdr = nullptr;
        return f(new_list);
    };
}

Expr string_fnct(const std::string &name, const Expr &args) {
    std::wstring s;
    if (is_a<Type::string>(args->car)) {
        s = args->car->string;
    } else if (is_a<Type::character>(args->car)) {
        s += args->car->chr;
    } else {
        s = s2ws(to_string(args->car));
    }
    if (name == "string-upcase") {
        transform(s.begin(), s.end(), s.begin(), ::toupper);
        return mk_string(s);
    } else if (name == "string-downcase") {
        transform(s.begin(), s.end(), s.begin(), ::tolower);
        return mk_string(s);
    }
    return mk_string(s);
}

Expr to_lower_str(const Expr &s) {
    auto t = std::wstring(s->string);
    transform(t.begin(), t.end(), t.begin(), ::tolower);
    return mk_string(t);
}

static std::function<bool(Char, Char)> eq_char = std::equal_to<Char>();
static std::function<bool(Char, Char)> neq_char = std::not_equal_to<Char>();
static std::function<bool(Char, Char)> gt_char = std::greater<Char>();
static std::function<bool(Char, Char)> ge_char = std::greater_equal<Char>();
static std::function<bool(Char, Char)> lt_char = std::less<Char>();
static std::function<bool(Char, Char)> le_char = std::less_equal<Char>();

PrimBasicFunct char_eq = predicate_chr<Char>(eq_char);
PrimBasicFunct char_neq = predicate_chr<Char>(neq_char);
PrimBasicFunct char_gt = predicate_chr<Char>(gt_char);
PrimBasicFunct char_ge = predicate_chr<Char>(ge_char);
PrimBasicFunct char_lt = predicate_chr<Char>(lt_char);
PrimBasicFunct char_le = predicate_chr<Char>(le_char);

Expr to_lower_char(const Expr &s) {
    return mk_char(tolower(s->chr));
}

} // namespace ax