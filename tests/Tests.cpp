#include "TestFramework.h"

#include <filesystem>

#include "LexerTests.h"
#include "ParserTests.h"
#include "SemaTests.h"
#include "CodegenTests.h"
#include "ErrorLoggingTests.h"

int main() {

    acorn::set_terminal_codepoint_to_utf8();

    test_lexer();

    test_parser();

    test_sema();

    test_error_logging();

    test_codegen();

    run_tests();

    return 0;
}