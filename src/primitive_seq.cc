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

Expr* const keyword_initial_element = mk_keyword(":initial-element");

template <typename T>
Expr* seq_length(const T& s)
{
    return mk_int(s.size());
}

Expr* length(Expr* args)
{
    if (is_false(args->car)) {
        return mk_int(0);
    }
    if (is_seq(args->car)) {
        if (is_a<Type::string>(args->car)) {
            return seq_length<String>(args->car->string);
        } else {
            return seq_length<Expr>(*args->car);
        }
    }
    throw EvalException("length: needs sequence argument");
}

Expr* elt(Expr* args)
{
    if (!is_a<Type::integer>(args->cdr->car)) {
        throw EvalException("elt: index is not a integer");
    }
    if (is_false(args->car)) {
        throw EvalException("elt: index out of range");
    }
    size_t index = args->cdr->car->integer;
    if (is_seq(args->car)) {
        if (is_a<Type::string>(args->car)) {
            auto seq = args->car->string;
            if (seq.size() <= index) {
                throw EvalException("elt: index out of range");
            }
            return mk_char(seq[index]);
        } else {
            auto seq = args->car;
            if (seq->size() <= index) {
                throw EvalException("elt: index out of range");
            }
            return (*seq)[index];
        }
    }
    throw EvalException("length: needs sequence argument");
}

Expr* subseq(Expr* args)
{
    if (is_false(args->car)) {
        return sF;
    }
    if (!is_a<Type::integer>(args->cdr->car)) {
        throw EvalException("elt: index is not a integer");
    }
    size_t index = args->cdr->car->integer;
    size_t length = 0;
    if (args->size() > 2) {
        if (!is_a<Type::integer>(args->cdr->cdr->car)) {
            throw EvalException("elt: length is not a integer");
        }
        length = args->cdr->cdr->car->integer;
        if (length <= 0) {
            throw EvalException("elt: length must be greater than zero");
        }
    }
    if (is_seq(args->car)) {
        if (is_a<Type::string>(args->car)) {
            String sub;
            auto seq = args->car->string;
            if (length == 0) {
                length = seq.size() - index;
            }
            copy_n(seq.begin() + index, length, back_inserter(sub));
            return mk_string(sub);
        } else {
            auto top = mk_list();
            auto s = top;
            auto seq = args->car;
            if (length == 0) {
                length = seq->size() - index;
            }
            Expr* prev = nullptr;
            for (unsigned int i = 0; i < args->car->size(); i++, seq = seq->cdr) {
                if (i >= index && i < index + length) {
                    s->car = seq->car;
                    s->cdr = mk_list();
                    prev = s;
                    s = s->cdr;
                }
            }
            if (prev)
                prev->cdr = nullptr;
            return top;
        }
    }
    throw EvalException("length: needs sequence argument");
};

template <typename T, typename S>
T seq_setelt(const string& name, T& s, size_t index, const S& r)
{
    if (index >= s->size()) {
        throw EvalException(name + ": index out of bounds");
    }
    (*s)[index] = r;
    return s;
}

void set_str_elt(const string& name, Expr* seq, Expr* c, size_t index)
{
    if (!is_seq(seq)) {
        throw EvalException(name + ": needs sequence argument");
    }
    if (!is_a<Type::character>(c)) {
        throw EvalException(name + ": strings need char replacement");
    }
    if (index >= seq->string.size()) {
        throw EvalException(name + ": index out of bounds");
    }
    seq->string[index] = c->chr;
}

void set_list_elt(const string& name, Expr* seq, Expr* c, size_t index)
{
    if (!is_seq(seq)) {
        throw EvalException(name + ": needs sequence argument");
    }
    if (index >= seq->size()) {
        throw EvalException(name + ": index out of bounds");
    }
    seq->set(index, c);
}

Expr* setelt(const string& name, Expr* args)
{
    auto seq = args->car;
    auto rindex = arg1(args);
    if (!is_a<Type::integer>(rindex)) {
        throw EvalException(name + ": needs integer index");
    }
    size_t index = rindex->integer;
    if (is_a<Type::string>(seq)) {
        set_str_elt(name, seq, arg2(args), index);
    } else {
        set_list_elt(name, seq, arg2(args), index);
    }
    return seq;
}

// setf version
// (var index) result
Expr* setf_elt(Evaluator& l, Expr* args, Expr* r, shared_ptr<SymbolTable> a)
{
    auto newargs = mk_list({ args->car, arg1(args), r });
    setelt("setf elt", newargs);
    return r;
}

Expr* make_sequence(Expr* args)
{
    if (!is_a<Type::atom>(args->car) || !is_seq_type(args->car->atom)) {
        throw EvalException("make-sequence: first argument must be a sequence type name");
    }
    if (!is_a<Type::integer>(arg1(args)) || !(arg1(args)->integer >= 0)) {
        throw EvalException("make-sequence: size must be a positive integer");
    }

    auto init = sF;
    if (auto opt = get_keyword_value(args, keyword_initial_element)) {
        init = *opt;
    }
    if (args->car->atom == type_string) {
        if (init == sF) {
            init = mk_char(' ');
        }
        if (!is_a<Type::character>(init)) {
            throw EvalException("make-sequence: :initial-element must be char");
        }
        return mk_string(String(arg1(args)->integer, Char(init->chr)));
    } else {
        return mk_list(arg1(args)->integer, init);
    }
}

Expr* concatenate_str(Expr* args)
{
    String result;
    for (auto ptr = args; !is_false(ptr); ptr = ptr->cdr) {
        if (is_a<Type::string>(ptr->car)) {
            result += ptr->car->string;
        } else if (is_a<Type::list>(ptr->car)) {
            for (auto x = ptr->car; !is_false(x); x = x->cdr) {
                if (is_a<Type::character>(x->car)) {
                    result += x->car->chr;
                } else {
                    throw EvalException("concatenate: strings concatenate only lists of chars");
                }
            }
        }
    }
    return mk_string(result);
}

Expr* concatenate_list(Expr* args)
{
    auto result = mk_list();
    auto cur = result;
    for (auto ptr = args; !is_false(ptr); ptr = ptr->cdr) {
        if (is_a<Type::list>(ptr->car)) {
            Expr* prev = nullptr;
            for (auto x = ptr->car; !is_false(x); x = x->cdr) {
                cur->car = x->car;
                cur->cdr = mk_list();
                prev = cur;
                cur = cur->cdr;
            }
            if (!prev)
                prev = nullptr;
        } else if (is_a<Type::string>(ptr->car)) {
            Expr* prev = nullptr;
            for (auto x : ptr->car->string) {
                cur->car = mk_char(x);
                cur->cdr = mk_list();
                prev = cur;
                cur = cur->cdr;
            }
            if (!prev)
                prev = nullptr;
        }
    }
    return result;
}

Expr* concatenate(Expr* args)
{
    if (!is_a<Type::atom>(args->car) || !is_seq_type(args->car->atom)) {
        throw EvalException("concatenate: first argument must be a sequence type name");
    }
    if (args->car->atom == type_string) {
        return concatenate_str(args->cdr);
    } else if (args->car->atom == type_list || args->car->atom == type_list2) {
        return concatenate_list(args->cdr);
    }
    throw EvalException("concatenate: can't concatenate type " + args->car->atom);
}
}