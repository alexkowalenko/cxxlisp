
# Benchmark programs

#option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
#add_subdirectory("${PROJECT_SOURCE_DIR}/extern/googletest" "extern/googletest")

option(BENCHMARK_ENABLE_TESTING "Enable testing of the benchmark library." OFF)
option(BENCHMARK_ENABLE_INSTALL "Enable installation of benchmark. (Projects embedding benchmark may want to turn this OFF.)" OFF)
add_subdirectory("${PROJECT_SOURCE_DIR}/extern/benchmark" "extern/benchmark")

include_directories(${PROJECT_SOURCE_DIR}/include)

macro(package_add_benchmark TESTNAME)
        #Add compile target
        add_executable(${TESTNAME} ${ARGN})

        target_link_libraries(${TESTNAME} PRIVATE project_options project_warnings
                                          cxxlispl replxx ${ICU_LIBRARIES} ${BOEHM_LIBRARIES} benchmark::benchmark)
endmacro() 

package_add_benchmark(bench_1 bench_1.cc)
package_add_benchmark(bench_2 bench_2.cc)
package_add_benchmark(bench_test bench_test.cc)
