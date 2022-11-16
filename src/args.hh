//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include "expr.hh"

#include <optional>
#include <string>

namespace ax {

enum class ArgConstraintType { no_check, none, eq, max, min };

enum class ArgType {
    any,
    numeric,
    integer,
    string,
    character,
};

struct ArgConstraint {
    ArgConstraintType constraint;
    unsigned int      num;
    ArgType           argType{ArgType::any};
};

const ArgConstraint one_arg = {ArgConstraintType::eq, 1};
const ArgConstraint two_args = {ArgConstraintType::eq, 2};
const ArgConstraint three_arg = {ArgConstraintType::eq, 3};

const ArgConstraint min_one = {ArgConstraintType::min, 1};
const ArgConstraint min_two = {ArgConstraintType::min, 2};
const ArgConstraint min_three = {ArgConstraintType::min, 3};

const ArgConstraint max_one = {ArgConstraintType::max, 1};
const ArgConstraint max_two = {ArgConstraintType::max, 2};

const ArgConstraint no_check = {ArgConstraintType::no_check, 0};

const ArgConstraint one_num = {ArgConstraintType::eq, 1, ArgType::numeric};
const ArgConstraint two_num = {ArgConstraintType::eq, 2, ArgType::numeric};
const ArgConstraint min_one_num = {ArgConstraintType::min, 1, ArgType::numeric};
const ArgConstraint any_num = {ArgConstraintType::no_check, 0, ArgType::numeric};

const ArgConstraint one_int = {ArgConstraintType::eq, 1, ArgType::integer};
const ArgConstraint two_int = {ArgConstraintType::eq, 2, ArgType::integer};

const ArgConstraint one_str = {ArgConstraintType::eq, 1, ArgType::string};
const ArgConstraint two_str = {ArgConstraintType::eq, 2, ArgType::string};
const ArgConstraint two_char = {ArgConstraintType::eq, 2, ArgType::character};

std::optional<std::string> checkArgs(const ArgConstraint &cons, const std::string &name,
                                     const Expr args);
} // namespace ax