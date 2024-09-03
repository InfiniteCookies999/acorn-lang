
# Find variables related to LLVM install.
find_package(LLVM REQUIRED CONFIG)

message(STATUS "INCLUDE DIRECTORIES FOR LLVM: ${LLVM_INCLUDE_DIRS}")

# Add LLVM includes to acorn.
include_directories(SYSTEM PUBLIC ${LLVM_INCLUDE_DIRS})

# separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
# Letting LLVM set some -D flags
add_definitions(${LLVM_DEFINITIONS})

# Add source to this project's executable.
add_library (acorn   "Logger.h" "Logger.cpp" "Lexer.h" "Lexer.cpp" "Source.h" "Token.h" "Token.cpp" "Parser.h" "Parser.cpp" "AST.h" "Identifier.h" "Identifier.cpp"    "PageAllocator.h" "Context.h" "Context.cpp" "Type.h" "Type.cpp" "Module.h" "Module.cpp" "Util.h" "Acorn.h" "Acorn.cpp" "AST.cpp" "LineTable.h" "LineTable.cpp" "ir/IRGen.h" "ir/IRGen.cpp" "ir/GenTypes.h" "ir/GenTypes.cpp"  "ir/GenOperator.cpp" "PageAllocator.cpp" "Sema.h" "Sema.cpp" "SourceExpansion.h" "SourceExpansion.cpp" "SourceFile.h" "Timer.h" "Timer.cpp"  "link/Linking.h" "link/Linking.cpp" "Util.cpp" "CodeGen.h" "CodeGen.cpp" "Process.h" "Process.cpp" "TypeTable.h" "TypeTable.cpp" "ErrorCodes.h")
# We add these include directories to the target so that targets that use the library also have
# access to the source files.
target_include_directories (acorn PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (acorn PUBLIC ${LLVM_INCLUDE_DIRS})

# TODO: This is probably most definitely, maybe? not the way this is suppose
#       to be done. Consults docs more carefully at some point.
#
# Let us see how many libs there are manually. We will just assume
# if it returns one then they installed llvm on a machine that joined
# them into a single library.
execute_process(
    COMMAND llvm-config --libs
    OUTPUT_VARIABLE llvm_config_out
    RESULT_VARIABLE llvm_config_result
)

if(llvm_config_result EQUAL 0)
    
    # Replace spaces with semicolons to create a list
    string(REPLACE " " ";" LIBS_LIST "${llvm_config_out}")

    # Convert the string into a list
    string(REPLACE ";" ";" LIBS_LIST "${LIBS_LIST}")

    # Check if the length of the list is 1
    list(LENGTH LIBS_LIST LIST_LENGTH)
    if(LIST_LENGTH EQUAL 1)
        set(USE_SINGLE_SHARED_LIB ON)
    else()
        set(USE_SINGLE_SHARED_LIB OFF)
    endif()

else()
    message(STATUS "NOT USING SHARED LIB")
    # Command failed just assuming it is installed normally.
    set(USE_SINGLE_SHARED_LIB OFF)
endif()

set(LLVM_USED_LIBS
    Analysis
    Core
    ExecutionEngine
    InstCombine
    Object
    OrcJIT
    RuntimeDyld
    Support
    TransformUtils
    native
)

if(USE_SINGLE_SHARED_LIB)

    llvm_config(acorn USE_SHARED ${LLVM_USED_LIBS})

else()

    llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_USED_LIBS})

    target_link_libraries(acorn ${LLVM_LIBS})

endif()

# Selecting which libraries to include.
# set(USED_LLVM_LIBS
#     Analysis
#     Core
#     ExecutionEngine
#     InstCombine
#     Object
#     OrcJIT
#     RuntimeDyld
#     Support
#     TransformUtils
#     native
# )

# For whatever reason on arch linux they package all the libraries into
# a single .so file so we use llvm_config(acorn USE_SHARED... to link instead
# if USE_SINGLE_SHARED_LIB is set. This should probably be done differently.
