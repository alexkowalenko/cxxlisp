//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <fstream>
#include <numeric>

#include <boost/algorithm/string/replace.hpp>

#include "exceptions.hh"

namespace ax {

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

inline Expr *mk_stream(fstream *const s, ios_base::openmode m) {
    auto e = new (GC) Expr(Type::stream);
    e->stream = new (GC) Stream();
    e->stream->str = s;
    if (m == ios_base::out) {
        e->stream->stream_type = StreamType::output;
    } else {
        e->stream->stream_type = StreamType::input;
    }
    return e;
}

//
// I/O functions
//

Expr *const keyword_direction = mk_keyword(":direction");
Expr *const keyword_input = mk_keyword(":input");
Expr *const keyword_output = mk_keyword(":output");

Expr *throw_error(Expr *args) {
    throw EvalException(to_string(args->car));
}

Expr *quit(Expr *args) {
    if (!is_false(args)) {
        if (is_a<Type::integer>(args)) {
            long ret = args->integer;
            throw ExceptionQuit(ret);
        }
    }
    throw ExceptionQuit(0);
}

Expr *open(Expr *args) {
    cout << "open args " << args << endl;
    if (is_a<Type::string>(args->car)) {
        auto filename = ws2s(args->car->string);

        ios_base::openmode dir = ios_base::out;
        if (auto opt = get_keyword_value(args, keyword_direction)) {
            if (is_a<Type::keyword>(*opt) && (*opt)->keyword == keyword_input->keyword) {
                cout << "open input " << endl;
                dir = ios_base::in;
            }
        }
        cout << "open mode = " << dir << endl;
        fstream *fs = new (GC) fstream();
        fs->open(filename.c_str(), dir);
        if (fs->bad()) {
            EvalException("open: problem opening " + filename);
        }
        auto res = mk_stream(fs, dir);
        return res;
    }
    throw EvalException("open: excepting filename as a string");
};

Expr *close(Expr *args) {
    if (is_a<Type::stream>(args->car)) {
        if (auto f = get_if<fstream *>(&args->car->stream->str)) {
            if ((*f)->is_open()) {
                (*f)->close();
                return sT;
            }
            throw EvalException("close: can't close closed stream");
        }
    }
    throw EvalException("close: not a stream");
}

Stream *get_output(const string &name, Expr *args, shared_ptr<SymbolTable> a) {
    Stream *output;
    if (args->size() == 1) {
        output = get_reference(name, std_out, a)->stream;
    } else {
        if (is_a<Type::stream>(arg1(args)) && arg1(args)->stream->is_output()) {
            output = arg1(args)->stream;
        } else {
            throw EvalException(name + ": argument is not an output stream");
        }
    }
    return output;
}

Expr *print(const string &name, Expr *args, shared_ptr<SymbolTable> a) {
    Stream *output = get_output(name, args, a);

    string out_str;
    if (name == "prin1" || name == "print") {
        if (name == "print") {
            out_str.push_back('\n');
        }
        out_str += to_string(args->car);
    } else if (name == "princ") {
        out_str += to_pstring(args->car);
    } else if (name == "write-char") {
        if (is_a<Type::character>(args->car)) {
            out_str += char(args->car->chr);
        } else {
            EvalException(name + ": argument is not a character");
        }
    }
    visit(
        overloaded{
            [&name](istream *) { throw EvalException(name + ": can't write to a input stream"); },
            [&out_str](ostream *arg) { (*arg) << out_str; },
            [&out_str](fstream *arg) { (*arg) << out_str; },
        },
        output->str);
    return args->car;
}

Expr *terpri(const string &name, Expr *args, shared_ptr<SymbolTable> a) {
    Stream *output;
    if (args->size() == 0) {
        output = get_reference(name, std_out, a)->stream;
    } else {
        if (is_a<Type::stream>(args->car) && args->car->stream->is_output()) {
            output = args->car->stream;
        } else {
            throw EvalException(name + ": argument is not an output stream");
        }
    }
    visit(
        overloaded{
            [&name](istream *) { throw EvalException(name + ": can't write to a input stream"); },
            [](fstream *arg) { (*arg) << endl; },
            [](ostream *arg) { (*arg) << endl; },
        },
        output->str);
    return sT;
}

Stream *get_input(const string &name, Expr *args, shared_ptr<SymbolTable> a) {
    Stream *input;
    if (args->size() == 0) {
        input = get_reference(name, std_in, a)->stream;
    } else {
        if (is_a<Type::stream>(args->car) && args->car->stream->is_input()) {
            input = args->car->stream;
        } else {
            throw EvalException(name + ": argument is not an input stream");
        }
    }
    return input;
};

Expr *read(const string &name, Expr *args, shared_ptr<SymbolTable> a) {
    Stream *         input = get_input(name, args, a);
    array<char, 255> buf;
    visit(
        overloaded{
            [&buf](istream *arg) { (*arg).getline(&buf[0], 255); },
            [&name](ostream *) { throw EvalException(name + ": can't write to a input stream"); },
            [&buf](fstream *arg) { (*arg).getline(&buf[0], 255); },
        },
        input->str);
    return mk_string(s2ws(string(&buf[0], strlen(&buf[0]))));
}

Expr *read_char(const string &name, Expr *args, shared_ptr<SymbolTable> a) {
    Stream *input = get_input(name, args, a);

    char c;
    visit(
        overloaded{
            [&c](istream *arg) { (*arg).get(c); },
            [&name](ostream *) { throw EvalException(name + ": can't write to a input stream"); },
            [&c](fstream *arg) { (*arg).get(c); },
        },
        input->str);
    return mk_char(Char(c));
}

Expr *format(const string &name, Expr *args, shared_ptr<SymbolTable> a) {
    if (!is_a<Type::string>(arg1(args))) {
        throw EvalException("format: format is not a string " + to_string(arg1(args)));
    }
    String format = arg1(args)->string;
    boost::replace_all(format, "~%", "\n");
    boost::replace_all(format, "~&", "\n");

    auto s_arg = args->cdr->cdr;
    for (size_t index = 1;; index++) {
        if (auto p = format.find('~', index); p != string::npos) {
            if (p + 1 < format.size()) {
                String sub;
                switch (format[p + 1]) {
                case 'S':
                    if (!is_false(s_arg)) {
                        sub = s2ws(to_string(s_arg->car));
                        s_arg = s_arg->cdr;
                    } else {
                        throw EvalException("format: no argument ~S " + std::to_string(index));
                    }
                    break;
                case 'A':
                    if (!is_false(s_arg)) {
                        sub = s2ws(to_pstring(s_arg->car));
                        s_arg = s_arg->cdr;
                    } else {
                        throw EvalException("format: no argument ~A " + std::to_string(index));
                    }
                    break;
                }
                format.replace(p, 2, sub);
            } else {
                throw EvalException("format: incomplete ~");
            }
        } else {
            // not found
            break;
        }
    }

    if (args->car == sF) {
        return mk_string(format);
    }

    Stream *output;
    if (args->car == sT) {
        output = get_reference("format", std_out, a)->stream;
    } else {
        if (is_a<Type::stream>(args->car) && args->car->stream->is_output()) {
            output = args->car->stream;
        } else {
            throw EvalException(name + ": argument is not an output stream");
        }
    }
    visit(
        overloaded{
            [&name](istream *) { throw EvalException(name + ": can't write to a input stream"); },
            [&format](ostream *arg) { (*arg) << ws2s(format); },
            [&format](fstream *arg) { (*arg) << ws2s(format); },
        },
        output->str);
    return sT;
}

Expr *trace(Evaluator &l, const string &, Expr *args, shared_ptr<SymbolTable>) {
    if (args->size() > 0) {
        for (auto cur = args; !is_false(cur); cur = cur->cdr) {
            if (!is_atom(cur->car)) {
                throw EvalException("trace: function is not an atom " + to_string(cur->car));
            }
            if (l.has_function(cur->car->atom)) {
                l.trace_functions.insert(cur->car->atom);
            } else {
                throw EvalException("trace: not a function " + cur->car->atom);
            }
        }
    }
    if (l.trace_functions.size() == 0) {
        return sF;
    }
    auto  top = mk_list();
    Expr *prev = nullptr;
    auto  cur = top;
    for (auto iter = l.trace_functions.begin(); iter != l.trace_functions.end(); iter++) {
        cur->car = mk_atom(*iter);
        cur->cdr = mk_list();
        prev = cur;
        cur = cur->cdr;
    }
    if (prev) {
        prev->cdr = nullptr;
    }
    return top;
}

Expr *untrace(Evaluator &l, const string &, Expr *args, shared_ptr<SymbolTable>) {
    if (args->size() > 0) {
        for (auto cur = args; !is_false(cur); cur = cur->cdr) {
            if (!is_atom(cur->car)) {
                throw EvalException("trace: function is not an atom " + to_string(cur->car));
            }
            if (auto iter = l.trace_functions.find(cur->car->atom);
                iter != l.trace_functions.end())
                l.trace_functions.erase(iter);
        }
    } else {
        // remove all function names
        l.trace_functions.clear();
    }
    return sT;
}

Expr *load(Evaluator &l, const string &name, Expr *args, shared_ptr<SymbolTable> a) {
    ifstream file;
    auto     filename = ws2s(args->car->string);
    file.open(filename);
    if (!file.is_open()) {
        throw EvalException("load: can't open " + filename);
    }
    auto output = get_reference(name, std_out, a)->stream;
    l.opt.push_options();
    l.opt.readline = false;
    l.repl(file, *get<ostream *>(output->str));
    l.opt.pop_options();
    return sT;
}
} // namespace ax