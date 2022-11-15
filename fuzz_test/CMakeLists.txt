#
# Common Lisp in C++17
#
# Copyright © Alex Kowalenko 2022.
#

include_directories(${PROJECT_SOURCE_DIR}/src)

macro(package_add_test TESTNAME)
    # create an exectuable in which the tests will be stored
    add_executable(${TESTNAME} ${ARGN})

    target_link_libraries(${TESTNAME} project_options
        project_warnings
        cxxlispl replxx
        ${ICU_LIBRARIES} ${BOEHM_LIBRARIES}
        -coverage
        -fsanitize=fuzzer,undefined,address)
    target_compile_options(${TESTNAME} PRIVATE -fsanitize=fuzzer,undefined,address,leak)
endmacro()

package_add_test(fuzz_lex fuzz_lex.cc)
package_add_test(fuzz_parse fuzz_parse.cc)

# Allow short runs during automated testing to see if something new breaks
set(FUZZ_RUNTIME
    10
    CACHE STRING "Number of seconds to run fuzz tests during ctest run") # Default of 10 seconds

add_test(NAME fuzz_lex COMMAND fuzz_lex -max_total_time=${FUZZ_RUNTIME} corpus)
add_test(NAME fuzz_parse COMMAND fuzz_parse -max_total_time=${FUZZ_RUNTIME} -only_ascii=1 corpus.parse)

execute_process(COMMAND "mkdir" -pv corpus corpus.parse WORKING_DIRECTORY fuzz_test)