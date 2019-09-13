//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <fstream>

#include "exceptions.hh"

namespace ax {

template <class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...)->overloaded<Ts...>;

inline Expr* mk_stream(fstream* const s, ios_base::openmode m)
{
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

Expr* const keyword_direction = mk_keyword(":direction");
Expr* const keyword_input = mk_keyword(":input");
Expr* const keyword_output = mk_keyword(":output");

Expr* throw_error(Expr* args)
{
    throw EvalException(to_string(args->car));
}

Expr* quit(Expr* args)
{
    if (!is_false(args)) {
        if (is_a<Type::integer>(args)) {
            long ret = args->integer;
            throw ExceptionQuit(ret);
        }
    }
    throw ExceptionQuit(0);
}

Expr* print(const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    Stream* output;
    if (args->size() == 1) {
        output = get_reference(name, std_out, a)->stream;
    } else {
        if (is_a<Type::stream>(arg1(args)) && arg1(args)->stream->is_output()) {
            output = arg1(args)->stream;
        } else {
            throw EvalException(name + ": argument is not an output stream");
        }
    }

    string out_str;
    if (name == "prin1" || name == "print") {
        if (name == "print") {
            out_str.push_back('\n');
        }
        out_str += to_string(args->car);
    } else {
        out_str += to_pstring(args->car);
    }
    cout << "write this : " << out_str << endl;
    visit(overloaded{
              [&name](istream* arg) {
                  throw EvalException(name + ": can't write to a input stream");
              },
              [&out_str](ostream* arg) { (*arg) << out_str; },
              [&out_str](fstream* arg) { (*arg) << out_str; },
          },
        output->str);
    return sT;
}

Expr* terpri(const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    Stream* output;
    if (args->size() == 0) {
        output = get_reference(name, std_out, a)->stream;
    } else {
        if (is_a<Type::stream>(args->car) && args->car->stream->is_output()) {
            output = args->car->stream;
        } else {
            throw EvalException(name + ": argument is not an output stream");
        }
    }
    visit(overloaded{
              [&name](istream* arg) {
                  throw EvalException(name + ": can't write to a input stream");
              },
              [](fstream* arg) { (*arg) << endl; },
              [](ostream* arg) { (*arg) << endl; },
          },
        output->str);
    return sT;
}

Expr* open(Expr* args)
{
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
        fstream* fs = new (GC) fstream();
        fs->open(filename.c_str(), dir);
        if (fs->bad()) {
            EvalException("open: problem opening " + filename);
        }
        auto res = mk_stream(fs, dir);
        return res;
    }
    throw EvalException("open: excepting filename as a string");
};

Expr* close(Expr* args)
{
    if (is_a<Type::stream>(args->car)) {
        if (auto f = get_if<fstream*>(&args->car->stream->str)) {
            if ((*f)->is_open()) {
                (*f)->close();
                return sT;
            }
            throw EvalException("close: can't close closed stream");
        }
    }
    throw EvalException("close: not a stream");
}
}