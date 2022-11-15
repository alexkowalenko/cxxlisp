
# Test programs build

include_directories(${PROJECT_SOURCE_DIR}/src)

mark_as_advanced(
    # BUILD_GMOCK BUILD_GTEST BUILD_SHARED_LIBS
    BUILD_GTEST BUILD_SHARED_LIBS
    # gmock_build_tests gtest_build_samples gtest_build_tests
    gtest_disable_pthreads gtest_force_shared_crt gtest_hide_internal_symbols
)

set_target_properties(gtest PROPERTIES FOLDER _deps)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")

add_library(testlib utils.cc)
target_link_libraries(testlib PRIVATE project_options
                                      project_warnings cxxlispl replxx gccpp ${ICU_LIBRARIES} gtest)
target_include_directories(testlib PUBLIC "${spdlog_SOURCE_DIR}/include")


macro(package_add_test TESTNAME)
        #Add compile target
        add_executable(${TESTNAME} ${ARGN})

        #link to Boost libraries AND your targets and dependencies
        target_link_libraries(${TESTNAME} PRIVATE project_options project_warnings
                                          cxxlispl fmt replxx gc gccpp testlib ${ICU_LIBRARIES}
                                          gtest gtest_main)

        add_test(NAME ${TESTNAME} 
                 COMMAND ${TESTNAME} )

endmacro() 


package_add_test(test_linereader test_linereader.cc)
package_add_test(test_token test_token.cc)
package_add_test(test_lexer test_lexer.cc)
package_add_test(test_parse test_parse.cc)

package_add_test(test_expr test_expr.cc)
package_add_test(test_symtable test_symtable.cc)

package_add_test(test_eval test_eval.cc)
package_add_test(test_functions test_functions.cc)
package_add_test(test_io test_io.cc)
package_add_test(test_numbers test_numbers.cc)
package_add_test(test_seq test_seq.cc)
package_add_test(test_stdlib test_stdlib.cc)
package_add_test(test_chars test_chars.cc)
package_add_test(test_string test_string.cc)
package_add_test(test_vector test_vector.cc)
package_add_test(test_winston test_winston.cc)