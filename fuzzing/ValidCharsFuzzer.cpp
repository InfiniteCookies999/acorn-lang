#include "ValidCharsFuzzer.h"

#include <random>

void fuzzer_only_valid_characters(std::ostream& ostream) {
    const int OUTPUT_CHARACTERS = 8000;

    std::random_device rd;
    std::mt19937 generator(rd());

    std::vector<char> allowed_characters;
    for (char c = 32; c <= 126; c++) {
        allowed_characters.push_back(c);
    }
    allowed_characters.push_back('\n');
    allowed_characters.push_back('\t');

    std::uniform_int_distribution<int> dist(0, allowed_characters.size() - 1);

    for (int i = 0; i < OUTPUT_CHARACTERS; i++) {
        char ch = allowed_characters[dist(generator)];
        ostream << ch;
    }
}