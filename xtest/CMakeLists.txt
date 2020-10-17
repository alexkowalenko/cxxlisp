#
# Cxx Scheme compiler
#
# Copyright © 2020 Alex Kowalenko
#

set(PYTHON_EXECUTABLE python3)
find_package(PythonInterp 3.7 REQUIRED)

macro(xtest_add_test TESTNAME)
    add_test(NAME xtest.${TESTNAME}
            COMMAND ${PYTHON_EXECUTABLE} ./runtests.py -t ${TESTNAME}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro()

# Parse tests

xtest_add_test(tests.parse)

# Run tests

xtest_add_test(tests.basic)
xtest_add_test(tests.clisp.basic)
xtest_add_test(tests.io)
xtest_add_test(tests.numbers)
xtest_add_test(tests.stdlib)
xtest_add_test(winston)