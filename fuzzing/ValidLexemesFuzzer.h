#ifndef VALID_LEXEMES_FUZZER_H
#define VALID_LEXEMES_FUZZER_H

#include <fstream>

#include "Compiler.h"

void fuzzer_valid_lexemes(std::ostream& ostream, acorn::Compiler& compiler);

#endif // VALID_LEXEMES_FUZZER_H