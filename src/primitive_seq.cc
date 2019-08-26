// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <utf8.h>

#include "exceptions.hh"
#include "function.hh"

namespace ax {

//
// Sequence functions
//

const Keyword keyword_initial_element(":initial-element");

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
    if (is_seq(args[0])) {
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
    if (is_seq(args[0])) {
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
    if (is_seq(args[0])) {
        if (is_a<String>(args[0])) {
            return seq_subseq<String>(args[0], index, length);
        } else {
            return seq_subseq<List>(args[0], index, length);
        }
    }
    throw EvalException("length: needs sequence argument");
};

template <typename T, typename S>
Expr seq_setelt(const string& name, Expr& s, size_t index, const S& r)
{
    if (index >= any_cast<T>(s).size()) {
        throw EvalException(name + ": index out of bounds");
    }
    any_cast<T&>(s)[index] = r;
    return s;
}

Expr setelt(List& args)
{
    auto seq = args[0];
    if (!is_seq(seq)) {
        throw EvalException("setelt: needs sequence argument");
    }
    auto rindex = args[1];
    if (!is_a<Int>(rindex)) {
        throw EvalException("setelt: needs integer index");
    }
    size_t index = any_cast<Int>(rindex);
    Expr res;
    if (is_a<String>(seq)) {
        if (!is_a<Char>(args[2])) {
            throw EvalException("setf elt: strings need char replacement");
        }
        res = seq_setelt<String, Char>("set-elt", seq, index, any_cast<Char>(args[2]));
    } else {
        res = seq_setelt<List, Expr>("set-elt", seq, index, args[2]);
    }
    return res;
}

Expr setf_elt(Evaluator& l, List& args, const Expr& r, SymbolTable& a)
{
    if (args.size() != 2) {
        throw EvalException("setf elt: incorrect number of arguments");
    }
    auto var = args[0];
    if (!is_a<Atom>(var)) {
        throw EvalException("setf elt: must be a reference");
    }
    if (auto seq = a.find(any_cast<Atom>(var))) {
        if (!is_seq(*seq)) {
            throw EvalException("setf elt: needs sequence argument");
        }
        auto rindex = l.eval(args[1], a);
        if (!is_a<Int>(rindex)) {
            throw EvalException("setf elt: needs integer index");
        }
        size_t index = any_cast<Int>(rindex);
        Expr res;
        if (is_a<String>(*seq)) {
            if (!is_a<Char>(r)) {
                throw EvalException("setf elt: strings need char replacement");
            }
            res = seq_setelt<String, Char>("setf elt", *seq, index, any_cast<Char>(r));
        } else {
            res = seq_setelt<List, Expr>("setf elt", *seq, index, r);
        }
        a.set(any_cast<Atom>(var), res);
        return res;
    } else {
        throw EvalException("setf elt: must be a reference");
    }
}

template <typename T, typename S>
void fill_seq(Expr& seq, const S& e)
{
    fill(any_cast<T&>(seq).begin(), any_cast<T&>(seq).end(), e);
}

Expr make_sequence(List& args)
{
    if (!is_a<Atom>(args[0]) || !is_seq_type(any_cast<Atom>(args[0]))) {
        throw EvalException("make-sequence: first argument must be a sequence type name");
    }
    if (!is_a<Int>(args[1]) || !(any_cast<Int>(args[1]) >= 0)) {
        throw EvalException("make-sequence: size must be a positive integer");
    }

    auto seq = make_type(any_cast<Atom>(args[0]), any_cast<Int>(args[1]));
    cout << to_string(seq) << endl;
    if (has_keyword(args, keyword_initial_element)) {
        Expr init = get_keyword_value(args, keyword_initial_element);
        if (is_a<String>(seq)) {
            if (!is_a<Char>(init)) {
                throw EvalException("make-sequence: :initial-element must be char");
            }
            fill_seq<String, Char>(seq, any_cast<Char>(init));
        } else {
            fill_seq<List, Expr>(seq, init);
        }
    }
    return seq;
}
}