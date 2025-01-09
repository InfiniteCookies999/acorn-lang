#include <iostream>
#include <random>

#include "Compiler.h"

#include "RandomFuzzer.h"
#include "ValidCharsFuzzer.h"
#include "ValidLexemesFuzzer.h"

int main() {

    acorn::PageAllocator allocator(acorn::get_system_page_size());
    acorn::Compiler compiler(allocator);
    compiler.set_stand_alone();
    compiler.set_max_error_count(1000);

#if 1
    std::ofstream ostream("fuzz.ac");
    if (!ostream) {
        std::cerr << "Failed to open file fuzz.ac for fuzzing\n";
        return 1;
    }

    //fuzzer_completely_random(ostream);
    //fuzzer_only_valid_characters(ostream);
    fuzzer_valid_lexemes(ostream, compiler);
    ostream.close(); // Close the file so that the compiler may open the file.
#endif

    acorn::Source source = {
        .path = L"fuzz.ac",
        .mod_name = "fuzz"
    };

    llvm::SmallVector<acorn::Source> sources;
    sources.push_back(source);
    compiler.run(sources);

    return 0;
}