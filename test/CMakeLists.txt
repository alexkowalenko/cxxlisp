
# Test programs build

include_directories(${PROJECT_SOURCE_DIR}/include)

file(GLOB test_src RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} test_*.cc)

add_library(testlib utils.cc)
target_link_libraries(testlib PRIVATE project_options project_warnings)

foreach(testSrc ${test_src})
        #Extract the filename without an extension (NAME_WE)
        get_filename_component(testName ${testSrc} NAME_WE)

        #Add compile target
        add_executable(${testName} ${testSrc})

        #link to Boost libraries AND your targets and dependencies
        target_link_libraries(${testName} PRIVATE project_options project_warnings
                                          cxxlispl replxx testlib ${ICU_LIBRARIES} ${BOEHM_LIBRARIES})

        add_test(NAME ${testName} 
                 COMMAND ${testName} )
endforeach(testSrc)