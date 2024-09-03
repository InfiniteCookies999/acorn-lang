
add_executable (tests "Tests.cpp" "TestFramework.cpp" "LexerTests.h" "LexerTests.cpp" "CodegenTests.h" "CodegenTests.cpp" "ParserTests.h" "ParserTests.cpp")

target_compile_definitions(tests PUBLIC TEST_SOURCE_DIR=\"${PROJECT_SOURCE_DIR}/tests/resources/\")

target_link_libraries(tests LINK_PUBLIC acorn)
