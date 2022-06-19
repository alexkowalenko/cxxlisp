
include_directories(../include)
include_directories(${REPLXX_INCLUDE_DIRS})
include_directories(${CLI11_INCLUDE_DIRS})

function(make_includable input_file output_file)
    file(READ ${input_file} content)
    set(delim "stdlib")
    set(content "R\"${delim}(\n${content})${delim}\"")
    file(WRITE ${output_file} "${content}")
endfunction(make_includable)

if (NOT EXISTS stdlib.lisp.hh)
        make_includable(stdlib.lisp stdlib.lisp.hh)
endif()

add_library(cxxlispl STATIC
    args.cc             lexer.cc            lisp.cc             primitive_funcs.cc  primitive_str.cc
    evaluator.cc        linereaderRL.cc     options.cc          primitive_io.cc     symboltable.cc
    expr.cc             linereaderReplxx.cc parser.cc           primitive_num.cc    token.cc
    function.cc         linereaderStream.cc primitive.cc        primitive_seq.cc    tracer.cc 
    stdlib.lisp.hh)

target_link_libraries(
    cxxlispl
    PRIVATE project_options
            project_warnings)