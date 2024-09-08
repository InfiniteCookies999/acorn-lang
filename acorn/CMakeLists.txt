
# Find variables related to LLVM install.
find_package(LLVM REQUIRED CONFIG)

# Add LLVM includes to acorn.
include_directories(SYSTEM PUBLIC ${LLVM_INCLUDE_DIRS})

# separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
# Letting LLVM set some -D flags
add_definitions(${LLVM_DEFINITIONS})

# Add source to this project's executable.
add_library (acorn   "Logger.h" "Logger.cpp" "Lexer.h" "Lexer.cpp" "Source.h" "Token.h" "Token.cpp"
                     "Parser.h" "Parser.cpp" "AST.h" "Identifier.h" "Identifier.cpp"    "PageAllocator.h"
                     "Context.h" "Context.cpp" "Type.h" "Type.cpp" "Module.h" "Module.cpp" "Util.h"
                     "Acorn.h" "Acorn.cpp" "AST.cpp" "LineTable.h" "LineTable.cpp" "ir/IRGen.h" "ir/IRGen.cpp"
                     "ir/GenTypes.h" "ir/GenTypes.cpp"  "ir/GenOperator.cpp" "PageAllocator.cpp"
                     "Sema.h" "Sema.cpp" "SourceExpansion.h" "SourceExpansion.cpp" "SourceFile.h"
                     "Timer.h" "Timer.cpp"  "link/Linking.h" "link/Linking.cpp" "Util.cpp" "CodeGen.h" "CodeGen.cpp"
                     "Process.h" "Process.cpp" "TypeTable.h" "TypeTable.cpp" "ErrorCodes.h" "ErrorCodesDef.inc")
# We add these include directories to the target so that targets that use the library also have
# access to the source files.
target_include_directories (acorn PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (acorn PUBLIC ${LLVM_INCLUDE_DIRS})

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

# If LLVM_LINK_LLVM_DYLIB is set to ON then llvm was built into a
# single library and and USE_SHARED option is required.
if(LLVM_LINK_LLVM_DYLIB)
    llvm_config(acorn USE_SHARED ${LLVM_USED_LIBS})
else()
    llvm_config(acorn ${LLVM_USED_LIBS})
endif()
