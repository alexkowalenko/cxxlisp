//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "options.hh"

#include <boost/program_options.hpp>
#include <iostream>

namespace ax {

namespace po = boost::program_options;
using namespace std;

Options getOptions(int argc, char* argv[])
{
    Options options;
    po::options_description desc("Allowed options");
    desc.add_options()(
        "help", "produce help message")(
        "silent", po::value<bool>(&(options.silent))->implicit_value(true), "silent, don't print the prompt")(
        "noreadline", po::value<bool>(&(options.readline))->implicit_value(false), "don't use readline for input");

    try {
        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);
        po::notify(vm);

        if (vm.count("help")) {
            cout << desc << endl;
            exit(EXIT_SUCCESS);
        }
    } catch (exception& e) {
        cerr << "error: " << e.what() << "\n";
    }

    return options;
}

} // namespace ax