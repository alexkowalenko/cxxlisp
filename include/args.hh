//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef ARGS_HH
#define ARGS_HH

#include "expr.hh"

#include <optional>
#include <string>

namespace ax {

using namespace std;

enum class ArgConstraintType {
    no_check,
    none,
    eq,
    max,
    min
};

struct ArgConstraint {
    ArgConstraintType type;
    int num;
};

const ArgConstraint one_arg = { ArgConstraintType::eq, 1 };
const ArgConstraint two_args = { ArgConstraintType::eq, 2 };
const ArgConstraint three_arg = { ArgConstraintType::eq, 3 };

const ArgConstraint min_one = { ArgConstraintType::min, 1 };
const ArgConstraint min_two = { ArgConstraintType::min, 2 };
const ArgConstraint min_three = { ArgConstraintType::min, 3 };

const ArgConstraint max_two = { ArgConstraintType::max, 2 };

const ArgConstraint no_check = { ArgConstraintType::no_check, 0 };

optional<string> checkArgs(const ArgConstraint& cons, const string& name, const List& args);
}

#endif