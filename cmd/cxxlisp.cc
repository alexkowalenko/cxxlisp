//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <clocale>
#include <iostream>

#include "lisp.hh"
#include "options.hh"

#include <boost/log/expressions.hpp>
#include <boost/log/support/date_time.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/utility/setup.hpp>

using namespace std;
using namespace ax;

namespace logging = boost::log;
namespace expr = boost::log::expressions;

void init_logging()
{
    // console sink
    auto sink = logging::add_console_log(std::cerr);

    sink->set_formatter(
        expr::stream << expr::format_date_time<boost::posix_time::ptime>("TimeStamp", "%Y-%m-%d %H:%M:%S.%f")
                     << ": [" << logging::trivial::severity
                     << "] " << expr::smessage);
    sink->imbue(sink->getloc());

    logging::add_common_attributes();

    logging::core::get()->set_filter(logging::trivial::severity >= logging::trivial::trace);
}

int main(int argc, char* argv[])
{
    // Get options
    ax::Options options = getOptions(argc, argv);
    setlocale(LC_ALL, "en_US.utf8");

    init_logging();

    Lisp lispInterp
        = Lisp(options);
    lispInterp.init();
    lispInterp.repl(cin, cout);
    lispInterp.terminate();

    return 0;
}