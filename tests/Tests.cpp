#include "TestFramework.h"

#include <filesystem>

#include "LexerTests.h"
#include "ParserTests.h"
#include "SemaTests.h"
#include "CodegenTests.h"

#include "Process.h"

int main() {

    test_lexer();
    
    test_parser();
    
    test_sema();

    test_codegen();
    
    run_tests();

    return 0;
}