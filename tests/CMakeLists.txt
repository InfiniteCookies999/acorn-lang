
add_executable (tests "Tests.cpp" "TestFramework.cpp" "LexerTests.h" "LexerTests.cpp" "CodegenTests.h" "CodegenTests.cpp" "ParserTests.h" "ParserTests.cpp" "SemaTests.cpp" "SemaTests.h" "resources/mock_std_lib/errors.ac" "ErrorLoggingTests.h" "ErrorLoggingTests.cpp")

target_compile_definitions(tests PUBLIC
    TEST_SOURCE_DIR=\"${PROJECT_SOURCE_DIR}/tests/resources/\"
    MULTITHREAD_ENABLED
)

# Win32 actually refers to windows OS that supports 32 and 64 bit.
if(WIN32)
    target_compile_definitions(tests PUBLIC UNICODE _UNICODE)
endif()
target_link_libraries(tests LINK_PUBLIC acorn)
