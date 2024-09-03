#include "TestFramework.h"

#include "LexerTests.h"
#include "ParserTests.h"
#include "CodegenTests.h"

int main() {

    test_lexer();
    
    test_parser();
    
    test_codegen();
    
    run_tests();

    return 0;
}