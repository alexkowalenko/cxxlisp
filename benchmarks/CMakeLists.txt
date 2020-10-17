
# Benchmark programs

file(GLOB bench_src RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} bench_*.cc)

foreach(benSrc ${bench_src})
        #Extract the filename without an extension (NAME_WE)
        get_filename_component(benName ${benSrc} NAME_WE)

        #Add compile target
        add_executable(${benName} ${benSrc})

        #link to Boost libraries AND your targets and dependencies
        target_link_libraries(${benName} libcxxlisp replxx ${ICU_LIBRARIES} ${BOEHM_LIBRARIES} ${GBEN_LIBRARIES})

endforeach(benSrc)