
# Benchmark programs

include_directories(${PROJECT_SOURCE_DIR}/include)

macro(package_add_benchmark TESTNAME)
        #Add compile target
        add_executable(${TESTNAME} ${ARGN})

        #link to Boost libraries AND your targets and dependencies
        target_link_libraries(${TESTNAME} PRIVATE project_options project_warnings
                                          cxxlispl replxx ${ICU_LIBRARIES} ${BOEHM_LIBRARIES} ${GBEN_LIBRARIES})
endmacro() 

package_add_benchmark(bench_1 bench_1.cc)
package_add_benchmark(bench_2 bench_2.cc)
package_add_benchmark(bench_test bench_test.cc)
