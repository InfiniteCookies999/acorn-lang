#include "TestFramework.h"

#include <filesystem>

#include "LexerTests.h"
#include "ParserTests.h"
#include "CodegenTests.h"

#include "Process.h"

int main() {

    //std::cout << (32 & 63 >> 2 ^ 12 * 94 - 32 / 3 >> 13 | ~11) << "\n";
    
    test_lexer();
    
    test_parser();
    
    test_codegen();
    
    run_tests();

    return 0;
}