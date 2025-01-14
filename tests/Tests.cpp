#include "TestFramework.h"

#include <filesystem>

#include "LexerTests.h"
#include "ParserTests.h"
#include "SemaTests.h"
#include "CodegenTests.h"

int main() {

    int a[][2] = {{1,2}, {3,4}};

    test_lexer();

    test_parser();

    test_sema();

    test_codegen();

    run_tests();

    return 0;
}