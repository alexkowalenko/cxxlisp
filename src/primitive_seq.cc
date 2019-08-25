// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <utf8.h>

#include "exceptions.hh"

namespace ax {

//
// Sequence functions
//

template <typename T>
Expr seq_length(const Expr& s)
{
    return Int(any_cast<T>(s).size());
}

Expr length(List& args)
{
    if (is_false(args[0])) {
        return Int{ 0 };
    }
    if (is_Seq(args[0])) {
        if (is_a<String>(args[0])) {
            return seq_length<String>(args[0]);
        } else {
            return seq_length<List>(args[0]);
        }
    }
    throw EvalException("length: needs sequence argument");
}

template <typename T>
Expr seq_elt(const Expr& s, size_t index)
{
    auto seq = any_cast<T>(s);
    if (seq.size() == 0 || seq.size() <= index) {
        throw EvalException("elt: index out of range");
    }
    return any_cast<T>(s)[index];
}

Expr elt(List& args)
{
    if (!is_a<Int>(args[1])) {
        throw EvalException("elt: index is not a integer");
    }
    if (is_false(args[0])) {
        throw EvalException("elt: index out of range");
    }
    size_t index = any_cast<Int>(args[1]);
    if (is_Seq(args[0])) {
        if (is_a<String>(args[0])) {
            return seq_elt<String>(args[0], index);
        } else {
            return seq_elt<List>(args[0], index);
        }
    }
    throw EvalException("length: needs sequence argument");
}

template <typename T>
Expr seq_subseq(const Expr& s, size_t index, size_t length)
{
    T sub;
    auto seq = any_cast<T>(s);
    if (length == 0) {
        length = seq.size() - index;
    }
    for (size_t i = index; i < index + length; i++) {
        sub.push_back(seq[i]);
    }
    return sub;
}

Expr subseq(List& args)
{
    if (is_false(args[0])) {
        return sF;
    }
    if (!is_a<Int>(args[1])) {
        throw EvalException("elt: index is not a integer");
    }
    size_t index = any_cast<Int>(args[1]);
    size_t length = 0;
    if (args.size() > 2) {
        if (!is_a<Int>(args[2])) {
            throw EvalException("elt: length is not a integer");
        }
        length = any_cast<Int>(args[2]);
        if (length <= 0) {
            throw EvalException("elt: length must be greater than zero");
        }
    }
    if (is_Seq(args[0])) {
        if (is_a<String>(args[0])) {
            string sub;
            auto seq = any_cast<String>(args[0]);
            if (length == 0) {
                length = seq.size() - index;
            }
            for (size_t i = index; i < index + length; i++) {
                utf8::append(seq[i], sub); // can't work this into String
            }
            return String(sub);
        } else {
            return seq_subseq<List>(args[0], index, length);
        }
    }
    throw EvalException("length: needs sequence argument");
};

Expr fill(List& args)
{
    return sF;
};
}