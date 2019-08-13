//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "options.hh"

#include <iostream>

#include <boost/log/trivial.hpp>
#include <boost/program_options.hpp>

namespace ax {

namespace po = boost::program_options;
using namespace std;

Options getOptions(int argc, char* argv[])
{
    Options options;
    po::options_description desc("Allowed options");
    string debug;

    desc.add_options()(
        "help,h", "produce help message")(
        "silent,s", po::value<bool>(&(options.silent))->implicit_value(true), "silent, don't print the prompt")(
        "noreadline,r", po::value<bool>(&(options.readline))->implicit_value(false), "don't use readline for input")(
        "parseonly,p", po::value<bool>(&(options.parse_only))->implicit_value(true), "only parse the input and print result")(
        "debug,D", po::value<string>(&debug)->implicit_value("", "debug options"));

    try {
        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);
        po::notify(vm);

        if (vm.count("help")) {
            cout << desc << endl;
            exit(EXIT_SUCCESS);
        }
        if (!debug.empty()) {
            if (debug.find('e')) {
                options.debug_expr = true;
                BOOST_LOG_TRIVIAL(debug) << "debug: show evaluation ";
            }
        }
    } catch (exception& e) {
        cerr << "error: " << e.what() << "\n";
    }

    return options;
}

} // namespace ax