#include "RandomFuzzer.h"

#include <random>

void fuzzer_completely_random(std::ofstream& ostream) {
    const int OUTPUT_CHARACTERS = 1000;

    std::random_device rd;
    std::mt19937 generator(rd());

    // Making sure to exclude 0 because zero in the middle of
    // the file just leads to boring results.
    std::uniform_int_distribution<int> dist(1, 255);

    for (int i = 0; i < OUTPUT_CHARACTERS; i++) {
        char ch = static_cast<char>(dist(generator));
        ostream << ch;
    }
}
